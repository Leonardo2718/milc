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

    -- returns string representations of the sub-trees of an AST node
    showSubTrees :: String -> a -> [String]

    -- returns string representation of an AST sub-tree
    showTree :: String -> a -> String
    showTree lead ast = intercalate "\n" (showAllTrees lead ast) where
        showAllTrees l t = (l ++ name) : showSubTrees l' t where
            name = nameOf t
            l' = ' ':' ':l

data (AbstractSyntaxTree a) => WithPos a = WithPos { subTree :: a, positionOf :: AlexPosn }
noPos = AlexPn (-1) (0) (0)

-- type of the AST root node
data AST = AST String Block

data Block = CodeBlock [WithPos Declaration]

data MType  = Int
            | Real
            | Char
            | Bool
            | UserType String
            deriving (Eq, Show)

data DeclSpec = DeclSpec { varName :: String, varDims :: [WithPos Int] }

data Declaration    = Vars { varSpecs :: [WithPos DeclSpec], varType :: WithPos MType }

-- AST data type of statements
data Statement = IfThenElse {stmtExpr :: Expression, thenBranch :: Statement, elseBranch :: Statement}
               | WhileDo {stmtExpr :: Expression, doStmt :: Statement}
               | Input {destID :: String}
               | Assign {destID :: String, stmtExpr :: Expression}
               | Write {writeExpr :: Expression}
               | Block {statements :: [Statement]}
               -- deriving (Eq)

data Operator = Add | Sub | Mul | Div deriving (Eq, Show)

-- AST data type of expressions
data Expression = Operator { op :: Operator, subExprL :: Expression, subExprR :: Expression }
                | Id { idName :: String }
                | IntVal { intValue :: Int }
                | RealVal { realValue :: Float }
                | CharVal { charValue :: Char }
                | BoolVal { boolValue :: Bool }
                -- deriving (Eq)

-- instantions of AST data types

instance AbstractSyntaxTree Int where
    nameOf = show
    showSubTrees _ _ = []

instance (AbstractSyntaxTree a) => AbstractSyntaxTree (WithPos a) where
    nameOf (WithPos a p) = concat2WithPadding 20 (nameOf a) pos where
        pos = concat ["(", showAlexPos p, ")"]
    showSubTrees l (WithPos a _) = showSubTrees l a

instance AbstractSyntaxTree AST where
    nameOf (AST f _) = "AST " ++ f ++ " "
    showSubTrees l (AST _ s) = [showTree l s]

instance AbstractSyntaxTree Block where
    nameOf (CodeBlock _) = "Block"
    showSubTrees l (CodeBlock ds) = map (showTree l) ds

instance AbstractSyntaxTree Declaration where
    nameOf (Vars _ t) = "Vars"
    showSubTrees l (Vars vars t) =  showTree l t : map (showTree l) vars

instance AbstractSyntaxTree DeclSpec where
    nameOf (DeclSpec name dims) = concat $ name : take (length dims) (repeat "[]")
    showSubTrees l (DeclSpec _ dims) = map (\ d -> showTree l d) dims

instance AbstractSyntaxTree MType where
    nameOf = show
    showSubTrees _ _ = []

instance AbstractSyntaxTree Statement where
    nameOf (IfThenElse _ _ _) = "IfThenElse"
    nameOf (WhileDo _ _)      = "WhileDo"
    nameOf (Input n)          = "Input " ++ show n
    nameOf (Assign n _)       = "Assign " ++ show n
    nameOf (Write _)          = "Write"
    nameOf (Block _)          = "Block"
    showSubTrees l (IfThenElse e th el)   = [showTree l e, showTree l th, showTree l el]
    showSubTrees l (WhileDo e s)          = [showTree l e, showTree l s]
    showSubTrees l (Input _)              = []
    showSubTrees l (Assign _ e)           = [showTree l e]
    showSubTrees l (Write e)              = [showTree l e]
    showSubTrees l (Block ss)             = map (showTree l) ss

instance AbstractSyntaxTree Expression where
    nameOf (Operator o _ _ )  = show o
    nameOf (Id n)     = "Id " ++ show n
    showSubTrees l (Id _) = []
    showSubTrees l e        = [showTree l (subExprL e), showTree l (subExprR e)]

instance Show AST where
    show = showTree ""

-- helper for logging the string representation of an AST sub-tree
logTree :: (AbstractSyntaxTree t, Monad m) => t -> CompilerMonadT () m
logTree = logBlock . showTree ""

-- helper for logging the first n lines of the string representation of
-- an AST sub-tree
logTreeLines :: (AbstractSyntaxTree t, Monad m) => Int -> t -> CompilerMonadT () m
logTreeLines n = logBlockLines n . showTree ""
