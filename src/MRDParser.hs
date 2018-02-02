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

import CompilerEnvironment
import MLexer

parseError :: String -> Token -> CompilerMonad a
parseError mkmsg t@(Token _ pos@(AlexPn _ l c)) = logError msg where
    msg = concat $ ["Parsing error at ", showAlexPos pos, ": ", mkmsg]

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

logTerminal :: Token -> [Token] -> CompilerMonad ()
logTerminal t ts = do
    logMsgLn ("-- found terminal: " ++ show t)
    logMsgLn ("   remaining tokens: " ++ showFirst 4 ts)

eatTerminal :: TokenType -> [Token] -> CompilerMonad [Token]
eatTerminal tt ts = case ts of
    [] -> compError ("Expecting to find " ++ show tt)
    t@(Token tt' _):ts' -> if tt == tt'
        then do
            logMsgLn ("-- eating terminal: " ++ show t)
            logMsgLn ("   remaining tokens: " ++ showFirst 4 ts')
            return ts'
        else parseError (concat ["Missing ", show tt, " token"]) t

parse :: [Token] -> CompilerMonad [Token]
parse ts = do
    logMsgLn "=== Running parser ==="
    parseStatement ts

parseStatement :: [Token] -> CompilerMonad [Token]
parseStatement ts = do
    logMsgLn "Looking for a Statement"
    stmt <- case ts of
        t@(Token IF _):ts' -> do
            logTerminal t ts'
            logMsgLn "-- parsing If Then Else statement"
            parseExpression ts' >>= eatTerminal THEN >>= parseStatement >>= eatTerminal ELSE >>= parseStatement
        t@(Token WHILE _):ts' -> do
            logTerminal t ts'
            logMsgLn "-- parsing While Do statement"
            parseExpression ts' >>= eatTerminal DO >>= parseStatement
        t1@(Token INPUT _):t2@(Token (ID _) _):ts' -> do
            logTerminal t1 (t2:ts')
            logTerminal t2 ts'
            logMsgLn "-- parsing Input statement"
            return ts'
        t1@(Token (ID _) _):t2@(Token ASSIGN _):ts' -> do
            logTerminal t1 (t2:ts')
            logTerminal t2 ts'
            logMsgLn "-- parsing Assignment"
            parseExpression ts'
        t@(Token WRITE _):ts' -> do
            logTerminal t ts'
            logMsgLn "-- parsing Write statement"
            parseExpression ts'
        t@(Token BEGIN _):ts' -> do
            logTerminal t ts'
            logMsgLn "-- parsing Block statement"
            parseStatementList ts'
        [] -> compError "Expecting more tokens to parse a Statement"
        t@(Token tt _):_ -> parseError ("Unexpected token: " ++ show tt) t
    logMsgLn "Found a Statement"
    return stmt

parseStatementList :: [Token] -> CompilerMonad [Token]
parseStatementList ts = logMsgLn "Looking for Sub-Statements" >> case ts of
    Token END _:ts' -> do
        logMsgLn "-- found end of Block statement"
        return ts'
    _ -> do
        stmt <- parseStatement ts
        logMsgLn "Statement is a Sub-Statement"
        eatTerminal SEMICOLON stmt >>= parseStatementList

parseExpression :: [Token] -> CompilerMonad [Token]
parseExpression [] = compError "Expecting more tokens to parse expression"
parseExpression ts = do
    logMsgLn "Looking for an Expression"
    parseSubExpression ts
    where
        parseSubExpression :: [Token] -> CompilerMonad [Token]
        parseSubExpression ts = do
            logMsgLn "-- looking for a Subexpression"
            parseTerm ts >>= eatAddOp
        parseTerm ts = do
            logMsgLn "-- looking for a Term"
            parseFactor ts >>= eatMulOp
        parseFactor ts = do
            logMsgLn "-- looking for a factor"
            ts' <- case ts of
                t1@(Token SUB _):t2@(Token (NUM _)_):ts' -> do
                    logTerminal t1 (t2:ts')
                    logTerminal t2 ts'
                    return ts'
                t@(Token LPAR _):ts' -> parseExpression ts' >>= eatTerminal RPAR
                t@(Token (ID _) _):ts' -> return ts'
                t@(Token (NUM _) _):ts' -> return ts'
                t@(Token tt _):ts'-> parseError ("Unexpected token: " ++ show tt) t
                [] -> logError "Expecting more tokens to parse expression"
            logMsgLn "-- found factor"
            return ts'
        eatAddOp (t:ts') = logTerminal t ts' >> case t of
            Token ADD _ -> parseExpression ts'
            Token SUB _ -> parseExpression ts'
            _           -> logMsgLn "---- Not part of Expression grammar: IGNORING" >> return (t:ts')
        eatAddOp ts' = return ts'
        eatMulOp (t:ts') = logTerminal t ts' >> case t of
            Token MUL _ -> parseTerm ts'
            Token DIV _ -> parseTerm ts'
            _           -> logMsgLn "---- Not part of Expression grammar: IGNORING" >> return (t:ts')
        eatMulOp ts' = return ts'
