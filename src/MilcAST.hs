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

data WithPos a  = WithPos { subTree :: a, positionOf :: AlexPosn }
                | WithEndPos { subTree :: a, positionOf :: AlexPosn, endPosition :: AlexPosn }
                deriving (Show)
instance (AbstractSyntaxTree a) => AbstractSyntaxTree (WithPos a) where
    nameOf (WithPos a p) = concat2WithPadding 20 (nameOf a) pos where
        pos = concat ["(", showAlexPos p, ")"]
    nameOf (WithEndPos a s e) = concat2WithPadding 20 (nameOf a) poses where
        poses = concat ["(", showAlexPos s, ") (", showAlexPos e ,")"]
    showSubTrees l (WithPos a _) = showSubTrees l a
    showSubTrees l (WithEndPos a _ _) = showSubTrees l a
noPos = AlexPn (-1) (0) (0)

-- type of the AST root node
data AST = AST String Scope
instance AbstractSyntaxTree AST where
    nameOf (AST f _) = "AST " ++ f
    showSubTrees l (AST _ s) = [showTree l s]
instance Show AST where
    show = showTree ""

data Scope = Scope [WithPos MDeclaration] [WithPos MStatement]
instance AbstractSyntaxTree Scope where
    nameOf (Scope _ _) = "Scope"
    showSubTrees l (Scope decls stmts) = map (showTree l) decls ++ map (showTree l) stmts

data MType  = Int
            | Real
            | Char
            | Bool
            | UserType String
            deriving (Eq, Show)
instance AbstractSyntaxTree MType where
    nameOf = show
    showSubTrees _ _ = []

data MParamDecl = MParamDecl String Int (WithPos MType) deriving (Show)
instance AbstractSyntaxTree MParamDecl where
    nameOf (MParamDecl n d (WithPos t _)) = concat $ n : take d (repeat "[]") ++ [" "] ++ [show t]
    showSubTrees _ _ = []

data MIdentifier = MIdName String deriving (Eq, Show)
instance AbstractSyntaxTree MIdentifier where
    nameOf = show
    showSubTrees _ _ = []

data MConstructor = MCtor String [WithPos MType]
instance AbstractSyntaxTree MConstructor where
    nameOf (MCtor name _) = name
    showSubTrees l (MCtor _ ts) = map (showTree l) ts

data MDestructor = MDtor String [WithPos MIdentifier]
instance AbstractSyntaxTree MDestructor where
    nameOf (MDtor name _) = name
    showSubTrees l (MDtor _ vs) = map (showTree l) vs

data DeclSpec = DeclSpec { varName :: String, varDims :: [WithPos MExpression] }
instance AbstractSyntaxTree DeclSpec where
    nameOf (DeclSpec name dims) = concat $ name : take (length dims) (repeat "[]")
    showSubTrees l (DeclSpec _ dims) = map (showTree l) dims

data MDeclaration   = Vars  { varSpecs :: [WithPos DeclSpec]
                            , varType :: WithPos MType
                            }
                    | Fun   { funName :: WithPos MIdentifier
                            , funType :: WithPos MType
                            , funParams :: [WithPos MParamDecl]
                            , funDecls :: [WithPos MDeclaration]
                            -- , funBody
                            }
                    | Data  { datatypeName :: (WithPos MIdentifier)
                            , datacCtors :: [WithPos MConstructor]
                            }
instance AbstractSyntaxTree MDeclaration where
    nameOf (Vars _ _) = "Vars"
    nameOf (Fun _ _ _ _) = "Fun"
    nameOf (Data _ _) = "Data"
    showSubTrees l (Vars vars t)    = showTree l t : map (showTree l) vars
    showSubTrees l (Fun n t ps ds)  = showTree l n : showTree l t : (map (showTree l) ps) ++ (map (showTree l) ds)
    showSubTrees l (Data name cts)  = showTree l name : map (showTree l) cts

data MCase = MCase MDestructor (WithPos MStatement)
instance AbstractSyntaxTree MCase where
    nameOf (MCase _ _) = "MCase"
    showSubTrees l (MCase dtor stmt) = [showTree l dtor, showTree l stmt]

-- AST data type of statements
data MStatement = IfThenElse {stmtExpr :: WithPos MExpression, thenBranch :: WithPos MStatement, elseBranch :: WithPos MStatement}
               | WhileDo {stmtExpr :: WithPos MExpression, doStmt :: WithPos MStatement}
               | CaseOf {stmtExpr :: WithPos MExpression, cases :: [WithPos MCase]}
               | Assign {destID :: WithPos MIdentifier, stmtExpr :: WithPos MExpression}
               | MRead {destID :: WithPos MIdentifier}
               | MPrint {printExpr :: WithPos MExpression}
               | CodeBlock {blockBody :: Scope}
instance AbstractSyntaxTree MStatement where
   nameOf (IfThenElse _ _ _) = "IfThenElse"
   nameOf (WhileDo _ _)      = "WhileDo"
   nameOf (CaseOf _ _)       = "CaseOf"
   nameOf (Assign _ _)       = "Assign "
   nameOf (MRead _)          = "MRead "
   nameOf (MPrint _)         = "MPrint"
   nameOf (CodeBlock scope)  = "CodeBlock (Scope)"
   showSubTrees l (IfThenElse e th el)  = [showTree l e, showTree l th, showTree l el]
   showSubTrees l (WhileDo e s)         = [showTree l e, showTree l s]
   showSubTrees l (CaseOf e cs)         = [showTree l e] ++ map (showTree l) cs
   showSubTrees l (Assign name expr)    = [showTree l name, showTree l expr]
   showSubTrees l (MRead name)          = [showTree l name]
   showSubTrees l (MPrint e)            = [showTree l e]
   showSubTrees l (CodeBlock scope)     = showSubTrees l scope

data MBinaryOp = MAdd | MSub | MMul | MDiv | MAnd | MOr deriving (Eq, Show)
data MConstant = IntConst Int | RealConst Float | CharConst Char | BoolConst Bool deriving (Eq, Show)

-- AST data type of expressions
data MExpression = BinaryOp { op :: MBinaryOp, subExprL :: MExpression, subExprR :: MExpression }
                | MId { idName :: MIdentifier }
                | ConstVal { constVal :: MConstant }
                -- deriving (Eq)
instance AbstractSyntaxTree MExpression where
    nameOf (BinaryOp o _ _ )= show o
    nameOf (MId mid)        = show mid
    nameOf (ConstVal v)     = show v
    showSubTrees l (MId _)      = []
    showSubTrees l (ConstVal _) = []
    showSubTrees l e            = [showTree l (subExprL e), showTree l (subExprR e)]

-- helper for logging the string representation of an AST sub-tree
logTree :: (AbstractSyntaxTree t, Monad m) => t -> CompilerMonadT () m
logTree = logBlock . showTree ""

-- helper for logging the first n lines of the string representation of
-- an AST sub-tree
logTreeLines :: (AbstractSyntaxTree t, Monad m) => Int -> t -> CompilerMonadT () m
logTreeLines n = logBlockLines n . showTree ""
