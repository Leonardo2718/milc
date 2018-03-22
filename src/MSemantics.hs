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
                 | MFunSym {funArgTypes :: [MType], returnType :: MType}
                 | MTypeSym
                 | MCtorSym {ctorArgTypes :: [MType], resultType :: MType}
                 deriving (Show, Eq)
data MSymbolEntry = MSymbolEntry {symName :: String, declPos :: AlexPosn, symInfo :: MSymbolInfo}
type MSymbolScope = HashMap.HashMap String MSymbolEntry
type MSymbolTable = [MSymbolScope]

instance Show MSymbolEntry where
    show e = "(" ++ symName e ++ ", " ++ showAlexPos (declPos e) ++ ", " ++ show (symInfo e) ++ ")"

showSymbolTable :: MSymbolTable -> String
showSymbolTable = intercalate "\n~~~\n" . Prelude.map showScope where
    showScope :: MSymbolScope -> String
    showScope = intercalate "\n" . Prelude.map showEntry . HashMap.toList
    showEntry :: (String, MSymbolEntry) -> String
    showEntry (s, e) = "  " ++ s ++ " -> " ++ show e

newtype MSemanticAnalyzerState = MSemanticAnalyzerState { symbolTable :: MSymbolTable }
type MSemanticAnalyzer a = CompilerMonadT a (State MSemanticAnalyzerState)

initSemanticAnalyzer :: MSemanticAnalyzerState
initSemanticAnalyzer = MSemanticAnalyzerState []

newScope :: MSymbolScope
newScope = HashMap.empty

getSymbolTable :: MSemanticAnalyzer MSymbolTable
getSymbolTable = do
    s <- get
    return . symbolTable $ s

setSymbolTable :: MSymbolTable -> MSemanticAnalyzer ()
setSymbolTable t = do
    s <- get
    put s{symbolTable = t}

popScope :: MSemanticAnalyzer MSymbolScope
popScope = do
    scope:table <- getSymbolTable
    setSymbolTable table
    return scope

pushScope :: MSymbolScope -> MSemanticAnalyzer ()
pushScope scope = do
    table <- getSymbolTable
    setSymbolTable (scope:table)

pushNewScope :: MSemanticAnalyzer ()
pushNewScope = pushScope newScope

defineSymbol :: String -> AlexPosn -> MSymbolInfo -> MSemanticAnalyzer ()
defineSymbol sym pos info = do
    scope <- popScope
    let scope' = HashMap.insert sym (MSymbolEntry sym pos info) scope
    pushScope scope'

isDefined :: String -> MSemanticAnalyzer Bool
isDefined sym = do
    table <- getSymbolTable
    return . contains sym $ table
    where
        contains :: String -> MSymbolTable -> Bool
        contains sym (s:ss) =  HashMap.member sym s || contains sym ss

lookupSymbol :: String -> MSemanticAnalyzer (Maybe MSymbolEntry)
lookupSymbol sym = do
    table <- getSymbolTable
    return . tableLookup sym $ table
    where
        tableLookup :: String -> MSymbolTable -> Maybe MSymbolEntry
        tableLookup sym (s:ss) = case HashMap.lookup sym s of
            Just e -> Just e
            Nothing -> tableLookup sym ss

logSymbolTable :: MSemanticAnalyzer ()
logSymbolTable = do
    table <- getSymbolTable
    logBlock (showSymbolTable table)

analyzeAST :: AST -> MSemanticAnalyzer ()
analyzeAST ast = do
    logMsgLn "=== Running semantic analysis ==="
    walkAST ast
    return ()
    where
        walkAST (AST _ scope) = do
            logMsgLn "Walking AST"
            walkDecls scope
        walkDecls (MScope decls _) = do
            logMsgLn "Walking declarations"


runSemanticAnalyzer :: Monad m => (AST -> MSemanticAnalyzer a) -> AST -> CompilerMonadT (a, MSemanticAnalyzerState) m
runSemanticAnalyzer analyzer ast = do
    let (c, s) = runState (runCompilerT . analyzer $ ast) initSemanticAnalyzer
    a <- compiler c
    return (a, s)
