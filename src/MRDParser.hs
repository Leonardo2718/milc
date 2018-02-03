{-

Copyright (C) 2018 Leonardo Banderali

License:

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.

-}

module MRDParser where

{-
The original Minisculus grammar:
```
prog -> stmt.
stmt -> IF expr THEN stmt ELSE stmt
      | WHILE expr DO stmt
      | INPUT ID
      | ID ASSIGN expr
      | WRITE expr
      | BEGIN stmtlist END.
stmtlist -> stmtlist stmt SEMICOLON
          |.
expr -> expr addop term
      | term.
addop -> ADD
      | SUB.
term -> term mulop factor
      | factor.
mulop -> MUL
      | DIV.
factor -> LPAR expr RPAR
        | ID
        | NUM
        | SUB NUM.
```

removing left recursion:
```
prog -> stmt.
stmt -> IF expr THEN stmt ELSE stmt
      | WHILE expr DO stmt
      | INPUT ID
      | ID ASSIGN expr
      | WRITE expr
      | BEGIN stmtlist END.
stmtlist -> stmt SEMICOLON stmtlist
          | .
expr -> term mexpr.
mexpr -> addop term mexpr
       | .
addop -> ADD
       | SUB.
term -> factor mterm.
mterm -> mulop factor mterm
       | .
mulop -> MUL
       | DIV.
factor -> LPAR expr RPAR
        | ID
        | NUM
        | SUB NUM.
```

simplifying:
```
prog -> stmt.
stmt -> IF expr THEN stmt ELSE stmt
      | WHILE expr DO stmt
      | INPUT ID
      | ID ASSIGN expr
      | WRITE expr
      | BEGIN stmtlist END.
stmtlist -> stmt SEMICOLON stmtlist
          | .
expr -> term addop expr
      | term .
addop -> ADD
       | SUB.
term -> factor mulop term.
      | factor.
mulop -> MUL
       | DIV.
factor -> LPAR expr RPAR
        | ID
        | NUM
        | SUB NUM.
```

-}

import Data.List
import Control.Monad.State

import CompilerEnvironment
import MLexer

data ParserState = ParserState  { compilerEnv       :: CompilerEnvironment
                                , remainingTokens   :: [Token]
                                , lastParsedToken   :: Token
                                }
type ParserStateMonad = State ParserState
type Parser a = CompilerMonadT a ParserStateMonad

initParserState :: CompilerEnvironment -> [Token] -> ParserState
initParserState env ts = ParserState {compilerEnv=env, remainingTokens=ts, lastParsedToken=EOF}

getEnv :: Parser CompilerEnvironment
getEnv = do
    s <- get
    return $ compilerEnv s

setEnv :: CompilerEnvironment -> Parser ()
setEnv env = do
    s <- get
    put (s{compilerEnv=env})

getTokens :: Parser [Token]
getTokens = do
    s <- get
    return $ remainingTokens s

setTokens :: [Token] -> Parser ()
setTokens ts = do
    s <- get
    put (s{remainingTokens=ts})

getLastParsedToken :: Parser Token
getLastParsedToken = do
    s <- get
    return $ lastParsedToken s

setLastParsedToken :: Token -> Parser ()
setLastParsedToken t = do
    s <- get
    put (s{lastParsedToken=t})

runParser :: CompilerEnvironment -> Parser a -> [Token] -> CompilerMonad (a, ParserState)
runParser env p ts = do
    let (c, s) = runState (runCompilerT p) (initParserState env ts)
    a <- compiler c
    return (a, s)

parseError :: String -> Token -> Parser a
parseError mkmsg t@(Token _ pos@(AlexPn _ l c)) = do
    -- env <- getEnv
    let msg = concat $ ["Parsing error at ", showAlexPos pos, ": ", mkmsg]--, "\n"
                       --, showErrorLocation (envSource env) l c]
    logError msg

showFirst :: Show a => Int -> [a] -> String
showFirst n l = if length l > n
    then concat ["[", intercalate ", " (map show firsts), ", ..."]
    else show firsts
    where
        firsts = take n l

data AST = AST Statement
data Statement = IfThenElse {stmtExpr :: Expression, thenBranch :: Statement, elseBranch :: Statement}
               | WhileDo {stmtExpr :: Expression, doStmt :: Statement}
               | Input {destID :: String}
               | Assign {destID :: String, stmtExpr :: Expression}
               | Write {sourceID :: String}
               | Block {statements :: [Statement]}
data Expression = Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Id String
                | Num Int

logTerminal :: Token -> [Token] -> Parser ()
logTerminal t ts = do
    logMsgLn ("-- found terminal: " ++ show t)
    logMsgLn ("   remaining tokens: " ++ showFirst 4 ts)

peekToken :: Parser Token
peekToken = do
    ts <- getTokens
    case ts of
        [] -> return EOF
        t:ts -> do
            logMsgLn ("-- peeking terminal: " ++ show t)
            return t

popToken :: Parser Token
popToken = do
    ts <- getTokens
    case ts of
        [] -> return EOF
        t:ts' -> do
            setTokens ts'
            setLastParsedToken t
            logMsgLn ("-- found terminal: " ++ show t)
            logMsgLn ("   remaining tokens: " ++ showFirst 4 ts')
            return t

eatTerminal :: TokenType -> Parser [Token]
eatTerminal tt = do
    t <- popToken
    case t of
        Token tt' _ -> if tt == tt'
            then do
                -- logMsgLn ("-- eating terminal: " ++ show t)
                ts <- getTokens
                -- logMsgLn ("   remaining tokens: " ++ showFirst 4 ts)
                return ts
            else parseError (concat ["Missing ", show tt, " token"]) t
        _ -> parseError ("Expecting to find " ++ show tt ++", got " ++ show t ++ " instead") t

parse :: CompilerEnvironment -> [Token] -> CompilerMonad [Token]
parse env ts = do
    logMsgLn "=== Running parser ==="
    (ts', _) <- runParser env parseStatement ts
    return ts'

parseStatement :: Parser [Token]
parseStatement = do
    logMsgLn "Looking for a Statement"
    t <- popToken
    stmt <- case t of
        Token IF _ -> do
            logMsgLn "-- parsing If Then Else statement"
            parseExpression >> eatTerminal THEN >> parseStatement >> eatTerminal ELSE >> parseStatement
        Token WHILE _ -> do
            logMsgLn "-- parsing While Do statement"
            parseExpression >> eatTerminal DO >> parseStatement
        Token INPUT _ -> do
            t' <- popToken
            ts' <- getTokens
            case t' of
                Token (ID _) _ -> logMsgLn "-- parsing Input statement" >> return ts'
                Token tt _ -> parseError ("Unexpected token: " ++ show tt ++ "\nExpected an ID") t'
        Token (ID _) _ -> do
            t' <- popToken
            ts' <- getTokens
            case t' of
                Token ASSIGN _ -> logMsgLn "-- parsing Assignment" >> parseExpression
                Token tt _ -> compError ("Unexpected token: " ++ show tt ++ "\nExpected an ASSIGNment operator")
        Token WRITE _ -> do
            logMsgLn "-- parsing Write statement"
            parseExpression
        Token BEGIN _ -> do
            logMsgLn "-- parsing Block statement"
            parseStatementList
        Token tt _ -> parseError ("Unexpected token: " ++ show tt) t
        EOF -> compError "Expecting more tokens to parse a Statement"
    logMsgLn "Found a Statement"
    return stmt

parseStatementList :: Parser [Token]
parseStatementList =  do
    logMsgLn "Looking for Sub-Statements"
    t <- peekToken
    case t of
        Token END _ -> do
            logMsgLn "-- found end of Block statement"
            popToken
            getTokens
        _ -> do
            stmt <- parseStatement
            logMsgLn "Statement is a Sub-Statement"
            eatTerminal SEMICOLON >> parseStatementList

parseExpression :: Parser [Token]
parseExpression = do
    logMsgLn "Looking for an Expression"
    parseSubExpression
    where
        parseSubExpression :: Parser [Token]
        parseSubExpression = do
            logMsgLn "-- looking for a Subexpression"
            parseTerm >> eatAddOp
        parseTerm :: Parser [Token]
        parseTerm = do
            logMsgLn "-- looking for a Term"
            parseFactor >> eatMulOp
        parseFactor :: Parser [Token]
        parseFactor = do
            logMsgLn "-- looking for a factor"
            t <- popToken
            ts <- getTokens
            ts' <- case t of
                Token SUB _ -> do
                    logTerminal t ts
                    t' <- popToken
                    ts' <- getTokens
                    logTerminal t' ts'
                    case t' of
                        Token (NUM _) _ -> return ts'
                        _ -> parseError ("Expecting to fint NUM, got " ++ show t' ++ " instead") t'
                Token LPAR _ -> parseExpression >> eatTerminal RPAR
                Token (ID _) _ -> return ts
                Token (NUM _) _ -> return ts
                _ -> parseError ("Unexpected " ++ show t) t
            logMsgLn "-- found factor"
            return ts'
        eatAddOp :: Parser [Token]
        eatAddOp = do
            t <- peekToken
            case t of
                Token ADD _ -> popToken >> parseExpression
                Token SUB _ -> popToken >> parseExpression
                _           -> logMsgLn "---- Not part of Expression grammar: IGNORING" >> getTokens
        eatMulOp :: Parser [Token]
        eatMulOp = do
            t <- peekToken
            case t of
                Token MUL _ -> popToken >> parseTerm
                Token DIV _ -> popToken >> parseTerm
                _           -> logMsgLn "---- Not part of Expression grammar: IGNORING" >> getTokens