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

module MSemantics where

import CompilerEnvironment
import MilcUtils
import MLexer
import MilcAST

import Control.Monad.State
import Data.HashMap.Strict as HashMap
import Data.List

data MSymbolInfo = MVarSym {symType :: MType, symDimension :: Int}
                 | MParamSym {symType :: MType, symDimension :: Int}
                 | MFunSym {funArgTypes :: [(MType, Int)], returnType :: MType}
                 | MTypeSym
                 | MCtorSym {ctorArgTypes :: [MType], resultTypeName :: String}
                 deriving (Show, Eq)
data MSymbolEntry = MSymbolEntry {symName :: String, declPos :: AlexPosn, symInfo :: MSymbolInfo}
type MSymbolScope = HashMap.HashMap String MSymbolEntry
type MSymbolTable = [MSymbolScope]

instance Show MSymbolEntry where
    show e = "(" ++ symName e ++ ", " ++ showAlexPos (declPos e) ++ ", " ++ show (symInfo e) ++ ")"

-- show symbol table as a string
showSymbolTable :: MSymbolTable -> String
showSymbolTable = intercalate "\n~~~\n" . Prelude.map showScope where
    showScope :: MSymbolScope -> String
    showScope = intercalate "\n" . Prelude.map showEntry . HashMap.toList
    showEntry :: (String, MSymbolEntry) -> String
    showEntry (s, e) = "  " ++ concat2WithPadding 20 (show s) (" -> " ++ show e)

newtype MSemanticAnalyzerState = MSemanticAnalyzerState { symbolTable :: MSymbolTable }
type MSemanticAnalyzer a = CompilerMonadT a (State MSemanticAnalyzerState)

-- initial state of the semantic analyzer
initSemanticAnalyzer :: MSemanticAnalyzerState
initSemanticAnalyzer = MSemanticAnalyzerState []

-- initial value of a new scope entry in the symbol table
newScope :: MSymbolScope
newScope = HashMap.empty

-- return current symbol table from the stats
getSymbolTable :: MSemanticAnalyzer MSymbolTable
getSymbolTable = do
    s <- get
    return . symbolTable $ s

-- set the symbol table in the current state
setSymbolTable :: MSymbolTable -> MSemanticAnalyzer ()
setSymbolTable t = do
    s <- get
    put s{symbolTable = t}

-- pop a scope from the symbol table
popScope :: MSemanticAnalyzer MSymbolScope
popScope = do
    scope:table <- getSymbolTable
    setSymbolTable table
    return scope

-- push a given scope onto the symbol table
pushScope :: MSymbolScope -> MSemanticAnalyzer ()
pushScope scope = do
    table <- getSymbolTable
    setSymbolTable (scope:table)

-- push a new (empty) scope onto the symbol table
pushNewScope :: MSemanticAnalyzer ()
pushNewScope = pushScope newScope

-- define a new symbol in the current scope of the symbol table
defineSymbol :: String -> AlexPosn -> MSymbolInfo -> MSemanticAnalyzer ()
defineSymbol sym pos info = do
    logMsgLn ("   adding " ++ show sym ++ " to symbol table")
    scope <- popScope
    let scope' = HashMap.insert sym (MSymbolEntry sym pos info) scope
    pushScope scope'

-- recursively check if a symbol is in the symbol table
isDefined :: String -> MSemanticAnalyzer Bool
isDefined sym = do
    table <- getSymbolTable
    return . contains sym $ table
    where
        contains :: String -> MSymbolTable -> Bool
        contains sym (s:ss) =  HashMap.member sym s || contains sym ss

-- retrieve the symbol table entry for a given symbol, if it exists, looking for
-- it recursively
lookupSymbol :: String -> MSemanticAnalyzer (Maybe MSymbolEntry)
lookupSymbol sym = do
    table <- getSymbolTable
    return . tableLookup sym $ table
    where
        tableLookup :: String -> MSymbolTable -> Maybe MSymbolEntry
        tableLookup sym (s:ss) = case HashMap.lookup sym s of
            Just e -> Just e
            Nothing -> tableLookup sym ss

-- log the current state of the symbol table
logSymbolTable :: MSemanticAnalyzer ()
logSymbolTable = do
    table <- getSymbolTable
    logBlock (showSymbolTable table)

-- run semantic analysis on an AST instance
analyzeAST :: AST -> MSemanticAnalyzer ()
analyzeAST ast = do
    logMsgLn "=== Running semantic analysis ==="
    pushNewScope
    walkAST ast
    return ()
    where
        walkAST :: AST -> MSemanticAnalyzer ()
        walkAST (AST _ scope) = do
            logMsgLn "Walking AST"
            walkDecls scope
        walkDecls :: MScope -> MSemanticAnalyzer ()
        walkDecls (MScope decls _) = do
            logMsgLn "Walking declarations"
            mapM (mapNoPos visitDecl) decls
            logMsgLn "Symbol table is now:"
            logSymbolTable
            mapM (mapNoPos walkSubDecls) decls
            logMsgLn "Symbol table is now:"
            logSymbolTable
            return ()
        visitDecl :: MDeclaration -> MSemanticAnalyzer ()
        visitDecl declaration = case declaration of
            Vars vars t -> do
                logMsgLn "-- found variable declarations"
                mapM visitVarDecl vars
                return ()
                where
                    visitVarDecl :: WithPos DeclSpec -> MSemanticAnalyzer ()
                    visitVarDecl decl = do
                        let name = mapNoPos varName decl
                            dims = mapNoPos (length . varDims) decl
                            pos = positionOf decl
                        defineSymbol name pos (MVarSym (removePos t) (dims))
            Fun n t ps _ _ -> do
                logMsgLn "-- found function declaration"
                let name = mapNoPos midName n
                    rtype = removePos t
                    params = Prelude.map (mapNoPos collectParams) ps
                    collectParams p = (removePos (paramType p), paramDim p)
                    pos = positionOf n
                defineSymbol name pos (MFunSym params rtype)
            Data n _ -> do
                logMsgLn "-- found data type declaration"
                let name = mapNoPos midName n
                    pos = positionOf n
                defineSymbol name pos MTypeSym
        walkSubDecls :: MDeclaration -> MSemanticAnalyzer ()
        walkSubDecls declaration = case declaration of
            Data n ctors -> do
                let name = mapNoPos midName n
                logMsgLn "-- walking sub-declarations of data type"
                logMsgLn ("   type is " ++ show name)
                mapM (visitCtor name) ctors
                return ()
            _ -> return ()
        visitCtor :: String -> WithPos MConstructor -> MSemanticAnalyzer ()
        visitCtor tname ctor = do
            let name = mapNoPos ctorName ctor
                pos = positionOf ctor
                types = Prelude.map (removePos) (mapNoPos ctorTypes ctor)
            defineSymbol name pos (MCtorSym types tname)

-- run semantic analysis in a compiler monad instance
runSemanticAnalyzer :: Monad m => (AST -> MSemanticAnalyzer a) -> AST -> CompilerMonadT (a, MSemanticAnalyzerState) m
runSemanticAnalyzer analyzer ast = do
    let (c, s) = runState (runCompilerT . analyzer $ ast) initSemanticAnalyzer
    a <- compiler c
    return (a, s)
