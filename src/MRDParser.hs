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
import MilcUtils
import MLexer


-- definition of the parser state monad type
data ParserState = ParserState  { compilerEnv       :: CompilerEnvironment  -- environment of current compilation
                                , remainingTokens   :: [Token]  -- stream of tokens being parsed
                                , lastParsedToken   :: Token    -- last token processed by parser
                                }
type ParserStateMonad = State ParserState
type Parser a = CompilerMonadT a ParserStateMonad

-- initializer for the the parser state
initParserState :: CompilerEnvironment -> [Token] -> ParserState
initParserState env ts = ParserState {compilerEnv=env, remainingTokens=ts, lastParsedToken=EOF}

-- getters and setters for the parser state
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

-- top level runner of a Parser instance
runParser :: Monad m => CompilerEnvironment -> Parser a -> [Token] -> CompilerMonadT (a, ParserState) m
runParser env p ts = do
    let (c, s) = runState (runCompilerT p) (initParserState env ts)
    a <- compiler c
    return (a, s)

-- throw a generic parsing error with the specified message
parseError :: String -> Parser a
parseError msg = logError $ concat ["Parse error: ", msg]

-- throw a parse error with the specified message and showing the specified
-- location in the source code
parseErrorAt :: AlexPosn -> String -> Parser a
parseErrorAt pos msg = do
    env <- getEnv
    let AlexPn _ l c = pos
        source = envSource env
    logError $ concat ["Parse error at ", showAlexPos pos, ":\n", msg, "\n", showErrorLocation source l c]

-- throw parse error indicating an unexpected token was found
badTokenError :: Token -> String -> Parser a
badTokenError (Token tt pos) msg = parseErrorAt pos $ concat ["Unexpected ", show tt, " ", msg, ":"]

-- throw parse error indicating a specific token type was expected but was
-- not found
--
-- also reports what token was found instead and at what position in the source
-- code
wrongTokenError :: TokenType -> Token -> Parser a
wrongTokenError expected actual = wrongTokenError' (show expected) actual

-- like `wrongTokenError` but takes the string representation of the expected
-- token type as first argument
wrongTokenError' :: String -> Token -> Parser a
wrongTokenError' expected actual = do
    lastToken <- getLastParsedToken
    case lastToken of
        EOF -> parseError $ concat ["Expecting ", expected, " but got ", show actual, " instead"]
        Token tt pos -> do
            env <- getEnv
            let source = envSource env
                AlexPn _ l c = pos
                Token tt' pos' = actual
                AlexPn _ l' c' = pos'
            parseError $ concat [ "\nExpecting ", expected, " to follow ", show tt, " at ", showAlexPos pos, ":\n"
                                , showErrorLocation source l c, "\n"
                                , "but got ", show tt', " instead at ", showAlexPos pos', ":\n"
                                , showErrorLocation source l' c'
                                ]

-- throw parse error indicating that a specific token type was expected but that
-- the token stream has ended
missingExpectedTokenError :: TokenType -> Parser a
missingExpectedTokenError expected = missingExpectedTokenError' (show expected)

-- like `missingExpectedTokenError` but takes the string representation of the
-- expected token type as first argument
missingExpectedTokenError' :: String -> Parser a
missingExpectedTokenError' expected = do
    lastToken <- getLastParsedToken
    case lastToken of
        EOF -> parseError ("Expecting " ++ expected ++ " token but got nothing.")
        Token tt p -> parseErrorAt p $ concat ["Expecting ", expected, " to follow ", show tt, " token but got nothing:"]

-- throw parse error indicating the token stream ended unexpectedly
-- (more tokens are needed to finish parsing)
missingTokenError :: Parser a
missingTokenError = do
    lastToken <- getLastParsedToken
    let msg = "Expecting another Token but got nothing. "
    case lastToken of
            EOF -> parseError msg
            Token _ p -> parseErrorAt p (msg ++ "Last token found here:")

-- assert that there are no tokens left to parse and throw an error if this is
-- not the case
assertNoMoreTokens :: Parser ()
assertNoMoreTokens = do
    ts <- getTokens
    case ts of
        [] -> return ()
        Token tt p:_ ->  parseErrorAt p $ concat ["Unexpected ", show tt, " (expected nothing):"]

-- type class specifying a common interface to elements of an AST
class AbstractSyntaxTree a where
    nameOf :: a -> String                   -- returns name of an AST node
    positionOf :: a -> AlexPosn             -- returns position in source code of an AST node

    -- returns string representations of the sub-trees of an AST node
    showSubTrees :: String -> a -> [String]

    -- returns string representation of an AST sub-tree
    showTree :: String -> a -> String
    showTree lead ast = intercalate "\n" (showAllTrees lead ast) where
        showAllTrees l t = concat [l, name, posPadding, "(", showAlexPos (positionOf t), ")"] : showSubTrees l' t where
            name = nameOf t
            posPadding = take (20 - length name) (repeat ' ')
            l' = ' ':' ':l

-- type of the AST root node
data AST = AST String Statement

-- AST data type of statements
data Statement = IfThenElse {stmtExpr :: Expression, thenBranch :: Statement, elseBranch :: Statement, stmtPos :: AlexPosn}
               | WhileDo {stmtExpr :: Expression, doStmt :: Statement, stmtPos :: AlexPosn}
               | Input {destID :: String, stmtPos :: AlexPosn}
               | Assign {destID :: String, stmtExpr :: Expression, stmtPos :: AlexPosn}
               | Write {writeExpr :: Expression, stmtPos :: AlexPosn}
               | Block {statements :: [Statement], stmtPos :: AlexPosn} deriving (Eq)

-- AST data type of expressions
data Expression = Add { subExprL :: Expression, subExprR :: Expression, exprPos :: AlexPosn}
                | Sub { subExprL :: Expression, subExprR :: Expression, exprPos :: AlexPosn}
                | Mul { subExprL :: Expression, subExprR :: Expression, exprPos :: AlexPosn}
                | Div { subExprL :: Expression, subExprR :: Expression, exprPos :: AlexPosn}
                | Id { idName :: String, exprPos :: AlexPosn}
                | Num { numValue :: Int, exprPos :: AlexPosn} deriving (Eq)

-- instantions of AST data types

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

-- helper for logging the string representation of an AST sub-tree
logTree :: (AbstractSyntaxTree t, Monad m) => t -> CompilerMonadT () m
logTree = logBlock . showTree ""

-- helper for logging the first n lines of the string representation of
-- an AST sub-tree
logTreeLines :: (AbstractSyntaxTree t, Monad m) => Int -> t -> CompilerMonadT () m
logTreeLines n = logBlockLines n . showTree ""

-- return the next token in the token stream without removing it
peekToken :: Parser Token
peekToken = do
    ts <- getTokens
    case ts of
        [] -> return EOF
        t:ts -> do
            logMsgLn ("-- peeking terminal: " ++ show t)
            return t

-- remove and return the next token in the token stream
popToken :: Parser Token
popToken = do
    ts <- getTokens
    case ts of
        [] -> missingTokenError
        t:ts' -> do
            setTokens ts'
            setLastParsedToken t
            logMsgLn ("-- popping terminal: " ++ show t)
            logMsgLn ("   remaining tokens: " ++ showFirst 4 ts')
            return t

-- remove and return the next token in the token stream, using the given
-- `extractor` to get any values held by the token
--
-- If the token given as argument is of the expected type, `extractor` must
-- return Just the value contained in the token (or `()` if the token contains
-- no value). Otherwise, `extractor` must return Nothing, which will result in
-- `eatValueToken` throwing `wrongTokenError`.
eatValueToken :: String -> (TokenType -> Maybe a) -> Parser a
eatValueToken name extractor = do
    ts <- getTokens
    case ts of
        [] -> missingExpectedTokenError' name
        t@(Token tt' _):ts' -> case extractor tt' of
            Just a -> do
                setTokens ts'
                setLastParsedToken t
                logMsgLn ("-- eating terminal: " ++ show t)
                logMsgLn ("   remaining tokens: " ++ showFirst 4 ts')
                return a
            Nothing -> wrongTokenError' name t

-- remove (without returning) the next token in the token stream if it is of the
-- specified type (`wrongTokenError` is thrown if it is not)
eatToken :: TokenType -> Parser ()
eatToken tt = eatValueToken (show tt) (\tt' -> if tt == tt' then Just () else Nothing)

-- parse the given token stream using the given compiler environment
parse :: Monad m => CompilerEnvironment -> [Token] -> CompilerMonadT (AST, ParserState) m
parse env ts = do
    logMsgLn "=== Running parser ==="
    p@(ast, _) <- runParser env parseProgram ts
    logMsgLn "Parsing successful"
    logTree ast
    return p

-- start parsing token stream at the top level
parseProgram :: Parser AST
parseProgram = do
    logMsgLn "Parsing program"
    stmt <- parseStatement
    assertNoMoreTokens
    env <- getEnv
    return $ AST (envSourceFile env) stmt

-- parse a statement from the token stream
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
            logMsgLn "-- parsing Input statement"
            n <- eatValueToken "ID" (\tt -> case tt of ID n -> Just n; _ -> Nothing;)
            return $ Input n p
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
        _ -> badTokenError t "while looking for a statement\n  (expected one of IF, WHILE, INPUT, ID, WRITE, or BEGIN)"
    logMsgLn $ "Found " ++ nameOf s ++ " statement"
    logTree s
    return s

-- parse a statement list
parseStatementList :: [Statement] -> Parser [Statement]
parseStatementList ss =  do
    logMsgLn "Inside Block statement"
    t <- peekToken
    case t of
        Token END _ -> do
            -- END is the only token in the follow set of `stmtlist`
            -- so if we find it, we know we're done :)
            logMsgLn "Found end of Block statement"
            popToken
            return ss
        _ -> do
            logMsgLn "Looking for Sub-Statements"
            stmt <- parseStatement
            logMsgLn "Statement is a Sub-Statement"
            eatToken SEMICOLON
            -- once a statement has been parsed and a SEMICOLON found, we can
            -- keep recursing down until we find an END token
            parseStatementList (ss ++ [stmt])

-- parse an expression
parseExpression :: Parser Expression
parseExpression = do
    logMsgLn "Looking for an Expression"
    e <- parseSubExpression
    logMsgLn "Found Expression"
    logTree e
    return e
    where
        -- parse an (sub-)expression
        parseSubExpression :: Parser Expression
        parseSubExpression = do
            logMsgLn "-- looking for a Subexpression"
            e <- parseTerm >>= eatAddOp
            logMsgLn "-- found Subexpression:"
            logTree e
            return e
        -- parse a term
        parseTerm :: Parser Expression
        parseTerm = do
            logMsgLn "-- looking for a Term"
            e <- parseFactor >>= eatMulOp
            logMsgLn "-- found Term"
            return e
        -- parse a factor
        parseFactor :: Parser Expression
        parseFactor = do
            logMsgLn "-- looking for a Factor"
            t <- popToken
            e <- case t of
                Token SUB p -> do
                    v <- eatValueToken "NUM" (\tt -> case tt of NUM v -> Just v; _ -> Nothing;)
                    return $ Num (-v) p
                Token LPAR _ -> do
                    e <- parseSubExpression
                    eatToken RPAR
                    return e
                Token (ID n) p -> return $ Id n p
                Token (NUM v) p -> return $ Num v p
                _ -> badTokenError t "while looking for an expression factor\n  (expected one of SUB, LPAR, ID, or NUM)"
            logMsgLn "-- found Factor"
            return e
        -- look for an ADD or SUB and remove if found, otherwise do nothing
        eatAddOp :: Expression -> Parser Expression
        eatAddOp e1 = do
            logMsgLn "-- looking for ADD or SUB"
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
                    logMsgLn "   is neither ADD nor SUB: IGNORING"
                    return e1
        -- look for a Mul or DIV and remove if found, otherwise do nothing
        eatMulOp :: Expression -> Parser Expression
        eatMulOp e1 = do
            logMsgLn "-- looking for MUL or DIV"
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
                    logMsgLn "   is neither MUL nor DIV: IGNORING"
                    return e1
