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

expectationError :: String -> String -> Parser a
expectationError expected actual = parseError $ concat ["Expecting ", expected, " but got ", actual, ":"]

wrongTokenError :: TokenType -> TokenType -> Parser a
wrongTokenError expected actual = expectationError (show expected) (show actual)

wrongTokenError' :: String -> TokenType -> Parser a
wrongTokenError' expected actual = expectationError expected (show actual)

missingExpectedTokenError :: TokenType -> Parser a
missingExpectedTokenError expected = do
    lastToken <- getLastParsedToken
    let more = case lastToken of
            EOF -> ""
            Token _ p -> " after Token at " ++ showAlexPos p
    expectationError (show expected) ("nothing" ++ more)

missingExpectedTokenError' :: String -> Parser a
missingExpectedTokenError' expected = do
    lastToken <- getLastParsedToken
    let more = case lastToken of
            EOF -> ""
            Token _ p -> " after Token at " ++ showAlexPos p
    expectationError expected ("nothing" ++ more)

missingTokenError :: Parser a
missingTokenError = do
    lastToken <- getLastParsedToken
    let more = case lastToken of
            EOF -> ""
            Token _ p -> " after Token at " ++ showAlexPos p
    expectationError "a Token" ("nothing" ++ more)

checkNoMoreTokens :: Parser ()
checkNoMoreTokens = do
    ts <- getTokens
    case ts of
        Token tt _:_ -> do
            t <- getLastParsedToken
            case t of
                Token tt' p -> parseError $ concat ["Expecting no more Tokens but got ", show tt, " after ", showAlexPos p, ":"]
                EOF -> parseError "MAJOR FAILURE: Found nothing to parse but unparsed tokens still exist!"
        _ -> return ()


showFirst :: Show a => Int -> [a] -> String
showFirst n l = if length l > n
    then concat ["[", intercalate ", " (map show firsts), ", ..."]
    else show firsts
    where
        firsts = take n l

class AbstractSyntaxTree a where
    nameOf :: a -> String
    positionOf :: a -> AlexPosn
    showSubTrees :: String -> a -> [String]
    showTree :: String -> a -> String
    showTree lead ast = intercalate "\n" (showAllTrees lead ast) where
        showAllTrees l t = concat [l, name, posPadding, "(", showAlexPos (positionOf t), ")"] : showSubTrees l' t where
            name = nameOf t
            posPadding = take (20 - length name) (repeat ' ')
            l' = ' ':' ':l

data AST = AST String Statement

data Statement = IfThenElse {stmtExpr :: Expression, thenBranch :: Statement, elseBranch :: Statement, stmtPos :: AlexPosn}
               | WhileDo {stmtExpr :: Expression, doStmt :: Statement, stmtPos :: AlexPosn}
               | Input {destID :: String, stmtPos :: AlexPosn}
               | Assign {destID :: String, stmtExpr :: Expression, stmtPos :: AlexPosn}
               | Write {writeExpr :: Expression, stmtPos :: AlexPosn}
               | Block {statements :: [Statement], stmtPos :: AlexPosn} deriving (Eq)
data Expression = Add { subExprL :: Expression, subExprR :: Expression, exprPos :: AlexPosn}
                | Sub { subExprL :: Expression, subExprR :: Expression, exprPos :: AlexPosn}
                | Mul { subExprL :: Expression, subExprR :: Expression, exprPos :: AlexPosn}
                | Div { subExprL :: Expression, subExprR :: Expression, exprPos :: AlexPosn}
                | Id { idName :: String, exprPos :: AlexPosn}
                | Num { numValue :: Int, exprPos :: AlexPosn} deriving (Eq)

instance AbstractSyntaxTree AST where
    nameOf (AST f _) = "AST " ++ f ++ " "
    positionOf (AST _ s) = positionOf s
    showSubTrees l (AST _ s) = [showTree l s]

instance AbstractSyntaxTree Statement where
    nameOf (IfThenElse _ _ _ _) = "IfThenElse"
    nameOf (WhileDo _ _ _)      = "WhileDo"
    nameOf (Input n _)          = "Input " ++ show n
    nameOf (Assign n _ _)       = "Assign " ++ show n
    nameOf (Write _ _)          = "Write"
    nameOf (Block _ _)          = "Block"
    positionOf                  = stmtPos
    showSubTrees l (IfThenElse e th el _)   = [showTree l e, showTree l th, showTree l el]
    showSubTrees l (WhileDo e s _)          = [showTree l e, showTree l s]
    showSubTrees l (Input _ _)              = []
    showSubTrees l (Assign _ e _)           = [showTree l e]
    showSubTrees l (Write e _)              = [showTree l e]
    showSubTrees l (Block ss _)             = map (showTree l) ss

instance AbstractSyntaxTree Expression where
    nameOf (Add _ _ _)  = "Add"
    nameOf (Sub _ _ _)  = "Sub"
    nameOf (Mul _ _ _)  = "Mul"
    nameOf (Div _ _ _)  = "Div"
    nameOf (Id n _)     = "Id " ++ show n
    nameOf (Num v _)    = "Num " ++ show v
    positionOf          = exprPos
    showSubTrees l (Id _ _) = []
    showSubTrees l (Num _ _)= []
    showSubTrees l e        = [showTree l (subExprL e), showTree l (subExprR e)]

instance Show AST where
    show = showTree ""

instance Show Statement where
    show = showTree ""

instance Show Expression where
    show = showTree ""

logTree :: AbstractSyntaxTree t => t -> Parser ()
logTree t = do
    logMsgLn "--------------------------------------------------"
    logMsgLn $ showTree "" t
    logMsgLn "--------------------------------------------------"

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
        [] -> missingTokenError
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
            then popToken >> logMsgLn "-- eating popped token" >> return ()
            else wrongTokenError tt tt'
        EOF -> missingExpectedTokenError tt

parse :: CompilerEnvironment -> [Token] -> CompilerMonad (AST, ParserState)
parse env ts = do
    logMsgLn "=== Running parser ==="
    p <- runParser env parseProgram ts
    logMsgLn "Parsing successful"
    return p

parseProgram :: Parser AST
parseProgram = do
    stmt <- parseStatement
    checkNoMoreTokens
    env <- getEnv
    return $ AST (envSourceFile env) stmt

parseStatement :: Parser Statement
parseStatement = do
    logMsgLn "Looking for a Statement"
    t <- popToken
    s <- case t of
        Token IF p -> do
            logMsgLn "-- parsing If Then Else statement"
            expr <- parseExpression
            eatToken THEN
            thenStmt <- parseStatement
            eatToken ELSE
            elseStmt <- parseStatement
            return $ IfThenElse expr thenStmt elseStmt p
        Token WHILE p -> do
            logMsgLn "-- parsing While Do statement"
            expr <- parseExpression
            eatToken DO
            stmt <- parseStatement
            return $ WhileDo expr stmt p
        Token INPUT p -> do
            t' <- peekToken
            case t' of
                Token (ID n) _ -> popToken >> logMsgLn "-- parsing Input statement" >> return (Input n p)
                Token tt _ -> wrongTokenError' "ID" tt
                EOF -> missingExpectedTokenError' "ID"
        Token (ID n) p -> do
            eatToken ASSIGN
            logMsgLn "-- parsing Assignment"
            e <- parseExpression
            return $ Assign n e p
        Token WRITE p -> do
            logMsgLn "-- parsing Write statement"
            e <- parseExpression
            return $ Write e p
        Token BEGIN p -> do
            logMsgLn "-- parsing Block statement"
            ss <- parseStatementList []
            return $ Block ss p
        Token tt _ -> wrongTokenError' "one of IF, WHILE, INPUT, ID, WRITE, or BEGIN" tt
    logMsgLn $ "Found " ++ nameOf s ++ " statement"
    logTree s
    return s

parseStatementList :: [Statement] -> Parser [Statement]
parseStatementList ss =  do
    logMsgLn "Inside Block statement"
    t <- peekToken
    case t of
        Token END _ -> do
            logMsgLn "Found end of Block statement"
            popToken
            return ss
        _ -> do
            logMsgLn "Looking for Sub-Statements"
            stmt <- parseStatement
            logMsgLn "Statement is a Sub-Statement"
            eatToken SEMICOLON
            parseStatementList (ss ++ [stmt])

parseExpression :: Parser Expression
parseExpression = do
    logMsgLn "Looking for an Expression"
    e <- parseSubExpression
    logMsgLn "Found Expression"
    logTree e
    return e
    where
        parseSubExpression :: Parser Expression
        parseSubExpression = do
            logMsgLn "-- looking for a Subexpression"
            e <- parseTerm >>= eatAddOp
            logMsgLn "-- found Subexpression:"
            logTree e
            return e
        parseTerm :: Parser Expression
        parseTerm = do
            logMsgLn "-- looking for a Term"
            e <- parseFactor >>= eatMulOp
            logMsgLn "-- found Term"
            return e
        parseFactor :: Parser Expression
        parseFactor = do
            logMsgLn "-- looking for a Factor"
            t <- popToken
            e <- case t of
                Token SUB p -> do
                    t' <- popToken
                    case t' of
                        Token (NUM v) _ -> return $ Num (-v) p
                        Token t' _ -> wrongTokenError' "NUM" t'
                Token LPAR _ -> do
                    e <- parseSubExpression
                    eatToken RPAR
                    return e
                Token (ID n) p -> return $ Id n p
                Token (NUM v) p -> return $ Num v p
                Token t' _ -> wrongTokenError' "one of SUB, LPAR, ID, or NUM" t'
            logMsgLn "-- found Factor"
            return e
        eatAddOp :: Expression -> Parser Expression
        eatAddOp e1 = do
            logMsgLn "-- looking for Add/Sub"
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
                    logMsgLn "----- Not part of Expression grammar: IGNORING"
                    return e1
        eatMulOp :: Expression -> Parser Expression
        eatMulOp e1 = do
            logMsgLn "-- looking for Mul/Div"
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
                    logMsgLn "----- Not part of Expression grammar: IGNORING"
                    return e1
