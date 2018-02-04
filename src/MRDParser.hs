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

parseError :: String -> Parser a
parseError mkmsg = do
    env <- getEnv
    t <- getLastParsedToken
    case t of
        Token _ pos@(AlexPn _ l c) -> logError (concat ["Parsing error at ", showAlexPos pos, ": ", mkmsg, "\n"
                                                   , showErrorLocation (envSource env) l c])
        EOF -> logError ("Parsing error: " ++ mkmsg)

showFirst :: Show a => Int -> [a] -> String
showFirst n l = if length l > n
    then concat ["[", intercalate ", " (map show firsts), ", ..."]
    else show firsts
    where
        firsts = take n l

data AST = AST Statement
data Statement = IfThenElse {stmtExpr :: Expression, thenBranch :: Statement, elseBranch :: Statement, stmtPos :: AlexPosn}
               | WhileDo {stmtExpr :: Expression, doStmt :: Statement, stmtPos :: AlexPosn}
               | Input {destID :: String, stmtPos :: AlexPosn}
               | Assign {destID :: String, stmtExpr :: Expression, stmtPos :: AlexPosn}
               | Write {sourceID :: String, stmtPos :: AlexPosn}
               | Block {statements :: [Statement], stmtPos :: AlexPosn} deriving (Eq)
data Expression = Add { subExprL :: Expression, subExprR :: Expression, exprPos :: AlexPosn}
                | Sub { subExprL :: Expression, subExprR :: Expression, exprPos :: AlexPosn}
                | Mul { subExprL :: Expression, subExprR :: Expression, exprPos :: AlexPosn}
                | Div { subExprL :: Expression, subExprR :: Expression, exprPos :: AlexPosn}
                | Id { idName :: String, exprPos :: AlexPosn}
                | Num { numValue :: Int, exprPos :: AlexPosn} deriving (Eq)

showExpr :: String -> Expression -> String
showExpr lead expr = intercalate "\n" (showSubExpr lead expr) where
    showSubExpr l e = case e of
        Add e1 e2 p -> concat [l, "Add\t\t(", showAlexPos p, ")"] : (showSubExpr (' ':' ':l) e1 ++ showSubExpr (' ':' ':l) e2)
        Sub e1 e2 p -> concat [l, "Sub\t\t(", showAlexPos p, ")"] : (showSubExpr (' ':' ':l) e1 ++ showSubExpr (' ':' ':l) e2)
        Mul e1 e2 p -> concat [l, "Mul\t\t(", showAlexPos p, ")"] : (showSubExpr (' ':' ':l) e1 ++ showSubExpr (' ':' ':l) e2)
        Div e1 e2 p -> concat [l, "Div\t\t(", showAlexPos p, ")"] : (showSubExpr (' ':' ':l) e1 ++ showSubExpr (' ':' ':l) e2)
        Id n p      -> [concat [l, "Id ", n, "\t\t(", showAlexPos p, ")"]]
        Num v p     -> [concat [l, "Num ", show v, "\t\t(", showAlexPos p, ")"]]

instance Show Expression where
    show e = showExpr "" e

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
            logMsgLn ("-- popped terminal: " ++ show t)
            logMsgLn ("   remaining tokens: " ++ showFirst 4 ts')
            return t

eatToken :: TokenType -> Parser ()
eatToken tt = do
    t <- peekToken
    case t of
        Token tt' _ -> if tt == tt'
            then popToken >> return ()
            else parseError (concat ["Expecting ", show tt, ", got ", show t, " instead"])
        _ -> parseError (concat ["Expecting ", show tt, ", got ", show t, " instead"])

parse :: CompilerEnvironment -> [Token] -> CompilerMonad [Token]
parse env ts = do
    logMsgLn "=== Running parser ==="
    (_, s) <- runParser env parseStatement ts
    return $ remainingTokens s

parseStatement :: Parser ()
parseStatement = do
    logMsgLn "Looking for a Statement"
    t <- popToken
    case t of
        Token IF _ -> do
            logMsgLn "-- parsing If Then Else statement"
            parseExpression >> eatToken THEN >> parseStatement >> eatToken ELSE >> parseStatement
        Token WHILE _ -> do
            logMsgLn "-- parsing While Do statement"
            parseExpression >> eatToken DO >> parseStatement
        Token INPUT _ -> do
            t' <- popToken
            case t' of
                Token (ID _) _ -> logMsgLn "-- parsing Input statement" >> return ()
                Token tt _ -> parseError ("Unexpected token: " ++ show tt ++ "\nExpected an ID")
        Token (ID _) _ -> do
            eatToken ASSIGN
            logMsgLn "-- parsing Assignment"
            parseExpression
        Token WRITE _ -> do
            logMsgLn "-- parsing Write statement"
            parseExpression
        Token BEGIN _ -> do
            logMsgLn "-- parsing Block statement"
            parseStatementList
        Token tt _ -> parseError ("Unexpected token: " ++ show tt)
        EOF -> parseError "Expecting more tokens to parse a Statement"
    logMsgLn "Found a Statement"

parseStatementList :: Parser ()
parseStatementList =  do
    logMsgLn "Looking for Sub-Statements"
    t <- peekToken
    case t of
        Token END _ -> do
            logMsgLn "-- found end of Block statement"
            popToken
            return ()
        _ -> do
            stmt <- parseStatement
            logMsgLn "Statement is a Sub-Statement"
            eatToken SEMICOLON
            parseStatementList

parseExpression :: Parser ()
parseExpression = do
    logMsgLn "Looking for an Expression"
    e <- parseSubExpression
    logMsgLn "Found Expression:"
    logMsgLn $ showExpr "  " e
    return ()
    where
        parseSubExpression :: Parser Expression
        parseSubExpression = do
            logMsgLn "-- looking for a Subexpression"
            parseTerm >>= eatAddOp
        parseTerm :: Parser Expression
        parseTerm = do
            logMsgLn "-- looking for a Term"
            parseFactor >>= eatMulOp
        parseFactor :: Parser Expression
        parseFactor = do
            logMsgLn "-- looking for a factor"
            t <- popToken
            e <- case t of
                Token SUB p -> do
                    t' <- popToken
                    case t' of
                        Token (NUM v) _ -> return $ Num (-v) p
                        _ -> parseError ("Expecting to fint NUM, got " ++ show t' ++ " instead")
                Token LPAR _ -> do
                    e <- parseSubExpression
                    eatToken RPAR
                    return e
                Token (ID n) p -> return $ Id n p
                Token (NUM v) p -> return $ Num v p
                _ -> parseError ("Unexpected " ++ show t)
            logMsgLn "-- found factor"
            return e
        eatAddOp :: Expression -> Parser Expression
        eatAddOp e1 = do
            t <- peekToken
            case t of
                Token ADD p -> do
                    popToken
                    e2 <- parseSubExpression
                    return $ Add e1 e2 p
                Token SUB p -> do
                    popToken
                    e2 <- parseSubExpression
                    return $ Sub e1 e2 p
                _           -> do
                    logMsgLn "---- Not part of Expression grammar: IGNORING"
                    return e1
        eatMulOp :: Expression -> Parser Expression
        eatMulOp e1 = do
            t <- peekToken
            case t of
                Token MUL p -> do
                    popToken
                    e2 <- parseTerm
                    return $ Mul e1 e2 p
                Token DIV p -> do
                    popToken
                    e2 <- parseTerm
                    return $ Div e1 e2 p
                _           -> do
                    logMsgLn "---- Not part of Expression grammar: IGNORING"
                    return e1
