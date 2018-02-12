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

{- this modules defines the milc AST -}

module MilcAST where

import CompilerEnvironment
import MilcUtils
import MLexer

import Data.List

-- type class specifying a common interface to elements of an AST
class AbstractSyntaxTree a where
    nameOf :: a -> String                   -- returns name of an AST node
    positionOf :: a -> AlexPosn             -- returns position in source code of an AST node

    -- returns string representations of the sub-trees of an AST node
    showSubTrees :: String -> a -> [String]

    -- returns string representation of an AST sub-tree
    showTree :: String -> a -> String
    showTree lead ast = intercalate "\n" (showAllTrees lead ast) where
        showAllTrees l t = (l ++ concat2WithPadding 20 name pos) : showSubTrees l' t where
            pos = concat ["(", showAlexPos (positionOf t), ")"]
            name = nameOf t
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
