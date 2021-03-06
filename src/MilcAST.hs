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

    mapNoPos :: (a -> c) -> WithPos a -> c
    mapNoPos f (WithPos a _) = f a

-- special AST node for storing the test position of a single child node
data WithPos a  = WithPos { removePos :: a, positionOf :: AlexPosn }
                | WithEndPos { removePos :: a, positionOf :: AlexPosn, endPosition :: AlexPosn }
                deriving (Show)
instance (AbstractSyntaxTree a) => AbstractSyntaxTree (WithPos a) where
    nameOf (WithPos a p) = concat2WithPadding 25 (nameOf a) pos where
        pos = concat ["(", showAlexPos p, ")"]
    nameOf (WithEndPos a s e) = concat2WithPadding 25 (nameOf a) poses where
        poses = concat ["(", showAlexPos s, ") (", showAlexPos e ,")"]
    showSubTrees l (WithPos a _) = showSubTrees l a
    showSubTrees l (WithEndPos a _ _) = showSubTrees l a

-- type of the AST root node
data AST = AST String MScope
instance AbstractSyntaxTree AST where
    nameOf (AST f _) = "AST " ++ f
    showSubTrees l (AST _ s) = [showTree l s]
instance Show AST where
    show = showTree ""

data MScope = MScope [WithPos MDeclaration] [WithPos MStatement]
instance AbstractSyntaxTree MScope where
    nameOf (MScope _ _) = "MScope"
    showSubTrees l (MScope decls stmts) = map (showTree l) decls ++ map (showTree l) stmts

data MType  = MInt
            | MReal
            | MChar
            | MBool
            | MUserType {typeName :: String}
            deriving (Eq, Show)
instance AbstractSyntaxTree MType where
    nameOf = show
    showSubTrees _ _ = []

data MParamDecl = MParamDecl {paramName::String, paramDim::Int, paramType::WithPos MType} deriving (Show)
instance AbstractSyntaxTree MParamDecl where
    nameOf (MParamDecl n d (WithPos t _)) = concat $ n : take d (repeat "[]") ++ [" "] ++ [show t]
    showSubTrees _ _ = []

data MIdentifier = MIdName {midName :: String} deriving (Eq, Show)
instance AbstractSyntaxTree MIdentifier where
    nameOf = show
    showSubTrees _ _ = []

data MConstructor = MCtor {ctorName::String, ctorTypes::[WithPos MType]}
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
                            , funBody :: [WithPos MStatement]
                            }
                    | Data  { datatypeName :: (WithPos MIdentifier)
                            , datacCtors :: [WithPos MConstructor]
                            }
instance AbstractSyntaxTree MDeclaration where
    nameOf (Vars _ _) = "Vars"
    nameOf (Fun _ _ _ _ _) = "Fun"
    nameOf (Data _ _) = "Data"
    showSubTrees l (Vars vars t)    = showTree l t : map (showTree l) vars
    showSubTrees l (Fun n t ps ds b)= showTree l n : showTree l t : (map (showTree l) ps) ++ (map (showTree l) ds) ++ (map (showTree l) b)
    showSubTrees l (Data name cts)  = showTree l name : map (showTree l) cts

data MCase = MCase MDestructor (WithPos MStatement)
instance AbstractSyntaxTree MCase where
    nameOf (MCase _ _) = "MCase"
    showSubTrees l (MCase dtor stmt) = [showTree l dtor, showTree l stmt]

data MLocation = MLocation {locName :: String, locOffsets :: [WithPos MExpression]}
instance AbstractSyntaxTree MLocation where
    nameOf (MLocation name offsets) = name ++ concat (take (length offsets) (repeat "[]"))
    showSubTrees l (MLocation _ offsets) = map (showTree l) offsets

-- AST data type of statements
data MStatement = IfThenElse {stmtExpr :: WithPos MExpression, thenBranch :: WithPos MStatement, elseBranch :: WithPos MStatement}
                | WhileDo {stmtExpr :: WithPos MExpression, doStmt :: WithPos MStatement}
                | CaseOf {stmtExpr :: WithPos MExpression, cases :: [WithPos MCase]}
                | Assign {destID :: WithPos MLocation, stmtExpr :: WithPos MExpression}
                | MRead {destID :: WithPos MLocation}
                | MPrint {printExpr :: WithPos MExpression}
                | CodeBlock {blockBody :: MScope}
                | MReturn {returnExpr :: WithPos MExpression}
instance AbstractSyntaxTree MStatement where
   nameOf (IfThenElse _ _ _) = "IfThenElse"
   nameOf (WhileDo _ _)      = "WhileDo"
   nameOf (CaseOf _ _)       = "CaseOf"
   nameOf (Assign _ _)       = "Assign "
   nameOf (MRead _)          = "MRead "
   nameOf (MPrint _)         = "MPrint"
   nameOf (CodeBlock scope)  = "CodeBlock (MScope)"
   nameOf (MReturn _)        = "Return"
   showSubTrees l (IfThenElse e th el)  = [showTree l e, showTree l th, showTree l el]
   showSubTrees l (WhileDo e s)         = [showTree l e, showTree l s]
   showSubTrees l (CaseOf e cs)         = [showTree l e] ++ map (showTree l) cs
   showSubTrees l (Assign name expr)    = [showTree l name, showTree l expr]
   showSubTrees l (MRead name)          = [showTree l name]
   showSubTrees l (MPrint e)            = [showTree l e]
   showSubTrees l (CodeBlock scope)     = showSubTrees l scope
   showSubTrees l (MReturn expr)        = [showTree l expr]

data MBinaryOp  = MAdd | MSub | MMul | MDiv
                | MAnd | MOr
                | MEqual | MLessThan | MLessEqual | MGreaterThan | MGreaterEqual
                deriving (Eq, Show)
data MUnaryOp   = MNeg
                | MNot
                | MFloat
                | MFloor
                | MCeil
                deriving (Eq, Show)
data MConstant = IntConst Int | RealConst Float | CharConst Char | BoolConst Bool deriving (Eq, Show)

-- AST data type of expressions
data MExpression = MBinaryOp { binOp :: MBinaryOp, subExprL :: WithPos MExpression, subExprR :: WithPos MExpression }
                 | MUnaryOp { unOp :: MUnaryOp, subExpr :: WithPos MExpression }
                 | MSize MIdentifier Int
                 | MCall { funId :: MIdentifier, funArgs :: [WithPos MExpression] }
                 | MCtorVal { ctorId :: MIdentifier, ctorArgs :: [WithPos MExpression] }
                 | MVar { varId :: MIdentifier, varDim :: [WithPos MExpression] }
                 | MConst { constVal :: MConstant }
instance AbstractSyntaxTree MExpression where
    nameOf (MBinaryOp o _ _ )   = show o
    nameOf (MUnaryOp o _)       = show o
    nameOf (MSize _ _)          = "MSize"
    nameOf (MCall _ _)          = "MCall"
    nameOf (MCtorVal _ _)       = "MCtor value"
    nameOf (MVar var _)         = "MVar"
    nameOf (MConst v)           = show v
    showSubTrees l (MBinaryOp _ lexpr rexpr)    = [showTree l lexpr, showTree l rexpr]
    showSubTrees l (MUnaryOp _ expr)            = [showTree l expr]
    showSubTrees l (MSize mid _)                = [showTree l mid]
    showSubTrees l (MCall fid args)             = showTree l fid : map (showTree l) args
    showSubTrees l (MCtorVal cid args)          = showTree l cid : map (showTree l) args
    showSubTrees l (MVar vid dims)              = showTree l vid : map (showTree l) dims
    showSubTrees l (MConst _)                   = []

-- helper for logging the string representation of an AST sub-tree
logTree :: (AbstractSyntaxTree t, Monad m) => t -> CompilerMonadT () m
logTree = logBlock . showTree ""

-- helper for logging the first n lines of the string representation of
-- an AST sub-tree
logTreeLines :: (AbstractSyntaxTree t, Monad m) => Int -> t -> CompilerMonadT () m
logTreeLines n = logBlockLines n . showTree ""
