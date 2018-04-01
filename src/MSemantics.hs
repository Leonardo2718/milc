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

import Data.HashMap.Strict as HashMap
import Data.List
import Control.Monad.State
import Control.Monad

data MSymbolInfo = MVarSym {symType :: MType, symDimension :: Int}
                 | MParamSym {symType :: MType, symDimension :: Int}
                 | MFunSym {funArgTypes :: [(MType, Int)], returnType :: MType, functionLabel :: String}
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

data MSemanticAnalyzerEnvironment = MSemanticAnalyzerEnvironment
    { compSource      :: String   -- the actual source code being compiled
    , compSourceFile  :: String   -- path to the file containing the source code (empty if using stdin)
    }
data MSemanticAnalyzerState = MSemanticAnalyzerState
    { symbolTable :: MSymbolTable               -- symbol table
    , functionLabelCounter :: Int               -- counter for generating unique function labels
    , compEnv :: MSemanticAnalyzerEnvironment   -- compilation environment
    }
type MSemanticAnalyzer a = CompilerMonadT a (State MSemanticAnalyzerState)

-- initial state of the semantic analyzer
initSemanticAnalyzer :: MSemanticAnalyzerEnvironment -> MSemanticAnalyzerState
initSemanticAnalyzer = MSemanticAnalyzerState [] 0

-- initial value of a new scope entry in the symbol table
newScope :: MSymbolScope
newScope = HashMap.empty

-- emit a semantic analysis error
semanticError :: AlexPosn -> String -> MSemanticAnalyzer a
semanticError pos@(AlexPn _ l c) msg = logError $ concat ["Semantic error at ", showAlexPos pos, ":\n", msg]

-- return current compilation environment
getEnv :: MSemanticAnalyzer MSemanticAnalyzerEnvironment
getEnv = do
    s <- get
    return . compEnv $ s

-- return a new, unique function label
getFunctionLabel :: String -> MSemanticAnalyzer String
getFunctionLabel name = do
    s <- get
    let counter = functionLabelCounter s
    put s{functionLabelCounter=counter+1}
    return (name ++ "_" ++ show counter)

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

-- recursively check if a symbol is in the symbol table
isDefined :: String -> MSemanticAnalyzer Bool
isDefined sym = do
    table <- getSymbolTable
    return . contains sym $ table
    where
        contains :: String -> MSymbolTable -> Bool
        contains _ [] = False
        contains sym (s:ss) =  HashMap.member sym s || contains sym ss

-- retrieve the symbol table entry for a given symbol, if it exists, looking for
-- it recursively
lookupSymbol :: String -> MSemanticAnalyzer (Maybe MSymbolEntry)
lookupSymbol sym = do
    table <- getSymbolTable
    return . tableLookup sym $ table
    where
        tableLookup :: String -> MSymbolTable -> Maybe MSymbolEntry
        tableLookup _ [] = Nothing
        tableLookup sym (s:ss) = case HashMap.lookup sym s of
            Just e -> Just e
            Nothing -> tableLookup sym ss

-- define a new symbol in the current scope of the symbol table
defineSymbol :: String -> AlexPosn -> MSymbolInfo -> MSemanticAnalyzer ()
defineSymbol sym pos info = do
    logMsgLn ("   adding " ++ show sym ++ " to symbol table")
    scope <- popScope
    case HashMap.lookup sym scope of
        Just entry -> do
            env <- getEnv
            let AlexPn _ l c = pos
                pos'@(AlexPn _ l' c') = declPos entry
                source = compSource env
            semanticError pos $ concat  [ "Redeclaration of ", show sym, " in current scope\n"
                                        , showCodeAt source l c, "\n"
                                        , "previous declaration at ", showAlexPos pos', ":\n"
                                        , showCodeAt source l' c'
                                        ]
        Nothing -> pushScope (HashMap.insert sym (MSymbolEntry sym pos info) scope)

-- redefine an existing symbol
redefineSymbol :: String -> AlexPosn -> MSymbolInfo -> MSemanticAnalyzer ()
redefineSymbol sym pos info = do
    logMsgLn ("   redefining symbol " ++ show sym)
    scope <- popScope
    let scope' = HashMap.insert sym (MSymbolEntry sym pos info) scope
    pushScope scope'

-- log the current state of the symbol table
logSymbolTable :: MSemanticAnalyzer ()
logSymbolTable = do
    table <- getSymbolTable
    logBlock (showSymbolTable table)

-- assert that a symbol is defined and if not, error out with given message
assertDefined :: String -> String -> AlexPosn -> MSemanticAnalyzer ()
assertDefined sym msg pos@(AlexPn _ l c) = do
    defined <- isDefined sym
    when (not defined) throwError
    where throwError = do
            env <- getEnv
            let source = compSource env
            semanticError pos $ concat [msg, "\n", showCodeAt source l c]

-- assert that if a data type is a user type, it is defined in the symbol table
assertTypeDefined :: WithPos MType -> MSemanticAnalyzer ()
assertTypeDefined t = case removePos t of
    UserType tname -> assertDefined tname ("Use of undeclared type " ++ show tname) (positionOf t)
    _ -> return ()

-- run semantic analysis on an AST instance
analyzeAST :: AST -> MSemanticAnalyzer ()
analyzeAST ast = do
    logMsgLn "=== Running semantic analysis ==="
    analyzeAST ast
    return ()
    where
        analyzeAST :: AST -> MSemanticAnalyzer ()
        analyzeAST (AST _ scope) = do
            logMsgLn "Analyzing AST"
            logMsgLn "Analyzing global scope"
            pushNewScope
            analyzeScope scope
        analyzeScope :: MScope -> MSemanticAnalyzer ()
        analyzeScope scope = do
            logMsgLn "Collecting type declarations"
            collectDecls typeCollector scope
            logMsgLn "Collecting value constructors"
            collectDecls ctorCollector scope
            logMsgLn "Collecting variables and functions"
            collectDecls varsAndFunCollector scope
        collectDecls :: (MDeclaration -> MSemanticAnalyzer ()) -> MScope -> MSemanticAnalyzer ()
        collectDecls collector (MScope decls _) = do
            mapM_ (mapNoPos collector) decls
            logMsgLn "SymbolTable is now:"
            logSymbolTable
        typeCollector :: MDeclaration -> MSemanticAnalyzer ()
        typeCollector decl = case decl of
            Data n _ -> do
                logMsgLn "-- found data type declaration"
                let name = mapNoPos midName n
                    pos = positionOf n
                defineSymbol name pos MTypeSym
            _ -> return ()
        ctorCollector :: MDeclaration -> MSemanticAnalyzer ()
        ctorCollector decl = case decl of
            Data tname ctors -> do
                let typeName = mapNoPos midName tname
                assertDefined typeName ("Expecting " ++ show typeName ++ " to have been defined but was not") (positionOf tname)
                logMsgLn ("-- found data type " ++ show typeName)
                mapM_ (visitCtor typeName) ctors
                where
                    visitCtor :: String -> WithPos MConstructor -> MSemanticAnalyzer ()
                    visitCtor tname ctor = do
                        logMsgLn "-- found value constructor"
                        let name = mapNoPos ctorName ctor
                            pos = positionOf ctor
                            types = Prelude.map removePos (mapNoPos ctorTypes ctor)
                        defineSymbol name pos (MCtorSym types tname)
            _ -> return ()
        varsAndFunCollector :: MDeclaration -> MSemanticAnalyzer ()
        varsAndFunCollector decl = case decl of
            Vars vars t -> do
                logMsgLn "-- found variable declarations"
                assertTypeDefined t
                mapM_ visitVar vars
                where
                    visitVar :: WithPos DeclSpec -> MSemanticAnalyzer ()
                    visitVar v = do
                        logMsgLn "-- found variable"
                        let name = mapNoPos varName v
                            dims = mapNoPos (length . varDims) v
                            pos = positionOf v
                        defineSymbol name pos (MVarSym (removePos t) dims)
            Fun n rt ps _ _ -> do
                logMsgLn "-- found function declaration"
                assertTypeDefined rt
                mapM_ (assertTypeDefined . paramType . removePos) ps
                let name = mapNoPos midName n
                    rtype = removePos rt
                    params = Prelude.map (mapNoPos collectParams) ps
                    collectParams p = (removePos (paramType p), paramDim p)
                    pos = positionOf n
                label <- getFunctionLabel name
                defineSymbol name pos (MFunSym params rtype label)
            _ -> return ()
-- run semantic analysis in a compiler monad instance
runSemanticAnalyzer :: Monad m => (AST -> MSemanticAnalyzer a) -> AST -> MSemanticAnalyzerEnvironment -> CompilerMonadT (a, MSemanticAnalyzerState) m
runSemanticAnalyzer analyzer ast env = do
    let (c, s) = runState (runCompilerT . analyzer $ ast) (initSemanticAnalyzer env)
    a <- compiler c
    return (a, s)
