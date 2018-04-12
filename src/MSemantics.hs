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
import MIL

import Data.HashMap.Strict as HashMap
import Data.List as List
import Control.Monad.State
import Control.Monad

data MVariableKind = MVariable | MParameter deriving (Show, Eq)
data MSymbolInfo = MVarSym {varType :: MType, varOffset :: Int, varDimension :: Int, varKind :: MVariableKind}
                 | MFunSym {funArgTypes :: [(MType, Int)], returnType :: MType, functionLabel :: String}
                 | MTypeSym
                 | MCtorSym {ctorArgTypes :: [MType], resultTypeName :: String}
                 deriving (Show, Eq)
data MSymbolEntry = MSymbolEntry
    { symName :: String
    , declPos :: AlexPosn
    , symInfo :: MSymbolInfo
    }
type MSymbolScope = HashMap.HashMap String MSymbolEntry
type MSymbolTable = [MSymbolScope]

instance Show MSymbolEntry where
    show e = "(" ++ symName e ++ ", " ++ showAlexPos (declPos e) ++ ", " ++ show (symInfo e) ++ ")"

-- show symbol table as a string
showSymbolTable :: MSymbolTable -> String
showSymbolTable = intercalate "\n  ~~~~~\n" . Prelude.map showScope where
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
    , generatedMil :: Mil                       -- the generated IL
    , functionLabelCounter :: Int               -- counter for generating unique function labels
    , basicBlockIdCounter :: BlockId            -- counter for generating unique basic block ids
    , compEnv :: MSemanticAnalyzerEnvironment   -- compilation environment
    }
type MSemanticAnalyzer a = CompilerMonadT a (State MSemanticAnalyzerState)

-- initial state of the semantic analyzer
initSemanticAnalyzer :: MSemanticAnalyzerEnvironment -> MSemanticAnalyzerState
initSemanticAnalyzer = MSemanticAnalyzerState [] (Mil []) 0 0

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

-- return a certain field from the compilation environment
fromEnv :: (MSemanticAnalyzerEnvironment -> a) -> MSemanticAnalyzer a
fromEnv field = do
    env <- getEnv
    return . field $ env

-- return a new, unique function label
getFunctionLabel :: String -> MSemanticAnalyzer String
getFunctionLabel name = do
    s <- get
    let counter = functionLabelCounter s
    put s{functionLabelCounter=counter+1}
    return (name ++ "_" ++ show counter)

-- generate a new basic block
newBasicBlock :: [OpCode] -> Terminator -> MSemanticAnalyzer BasicBlock
newBasicBlock opcodes terminator = do
    s <- get
    let bbid = basicBlockIdCounter s
    put s{basicBlockIdCounter=bbid+1}
    return $ BasicBlock bbid opcodes terminator

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

-- get scope at the top of the scope stack
getTopScope :: MSemanticAnalyzer MSymbolScope
getTopScope = do
    table <- getSymbolTable
    when (List.null table) (logError "Attempting to access scope when none is currently defined")
    return . head $ table

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
pushNewScope = logMsgLn "-- pushing new scope to symbol table" >> pushScope newScope

-- count the number of variables defined in the current scope
countVariables :: MSemanticAnalyzer Int
countVariables = do
    scope <- getTopScope
    let vars = HashMap.filter isVar scope
        isVar sym = case symInfo sym of
            MVarSym _ _ _ MVariable -> True
            _ -> False
    return . size $ vars

-- count the number of parameters defined in the current scope
countParameters :: MSemanticAnalyzer Int
countParameters = do
    scope <- getTopScope
    let vars = HashMap.filter isVar scope
        isVar sym = case symInfo sym of
            MVarSym _ _ _ MParameter -> True
            _ -> False
    return . size $ vars

-- push IL function
pushMilFunction :: Function -> MSemanticAnalyzer ()
pushMilFunction f = do
    s <- get
    let Mil fs = generatedMil s
        mil = Mil (f:fs)
    put s{generatedMil=mil}

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
lookupMaybe :: String -> MSemanticAnalyzer (Maybe MSymbolEntry)
lookupMaybe sym = do
    table <- getSymbolTable
    return . tableLookup sym $ table
    where
        tableLookup :: String -> MSymbolTable -> Maybe MSymbolEntry
        tableLookup _ [] = Nothing
        tableLookup sym (s:ss) = case HashMap.lookup sym s of
            Just e -> Just e
            Nothing -> tableLookup sym ss

lookupWithLevelMaybe :: String -> MSemanticAnalyzer (Maybe (MSymbolEntry, Int))
lookupWithLevelMaybe sym = do
    table <- getSymbolTable
    return . tableLookup 0 sym $ table
    where
        tableLookup :: Int -> String -> MSymbolTable -> Maybe (MSymbolEntry, Int)
        tableLookup _ _ [] = Nothing
        tableLookup i sym (s:ss) = case HashMap.lookup sym s of
            Just e -> Just (e, i)
            Nothing -> tableLookup (i+1) sym ss

-- retrieve the symbol table entry for a given symbol, looking recursively and
-- throwing an error if it's not found
lookupSymbol :: String -> String -> AlexPosn -> MSemanticAnalyzer MSymbolEntry
lookupSymbol what sym pos@(AlexPn _ l c) = do
    maybeEntry <- lookupMaybe sym
    source <- fromEnv compSource
    case maybeEntry of
        Just entry -> return entry
        Nothing -> semanticError pos $ concat
            [ "No ", what, " named ", show sym, " in current scope\n"
            , showCodeAt source l c
            ]

-- retrieve the symbol table entry for a given symbol, looking recursively and
-- throwing an error if it's not found
lookupWithLevel :: String -> String -> AlexPosn -> MSemanticAnalyzer (MSymbolEntry, Int)
lookupWithLevel what sym pos@(AlexPn _ l c) = do
    maybeEntry <- lookupWithLevelMaybe sym
    source <- fromEnv compSource
    case maybeEntry of
        Just entry -> return entry
        Nothing -> semanticError pos $ concat
            [ "No ", what, " named ", show sym, " in current scope\n"
            , showCodeAt source l c
            ]


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

-- a generic assert mechanism on a boolean value
assertThat :: Bool -> AlexPosn -> String -> MSemanticAnalyzer ()
assertThat val pos msg = unless val $ semanticError pos msg

-- assert that a symbol is defined and if not, error out with given message
assertDefined :: String -> AlexPosn -> String -> MSemanticAnalyzer ()
assertDefined sym pos@(AlexPn _ l c) msg = do
    defined <- isDefined sym
    unless defined throwError
    where throwError = do
            env <- getEnv
            let source = compSource env
            semanticError pos $ concat [msg, "\n", showCodeAt source l c]

-- assert that if a data type is a user type, it is defined in the symbol table
assertTypeDefined :: WithPos MType -> MSemanticAnalyzer ()
assertTypeDefined t = case removePos t of
    MUserType tname -> assertDefined tname (positionOf t) ("Use of undeclared type " ++ show tname)
    _ -> return ()

-- throw error signalling the use of an unimplemented language feature
unimplementedFeatureError :: AlexPosn -> String -> MSemanticAnalyzer a
unimplementedFeatureError pos@(AlexPn _ l c) feature = do
    source <- fromEnv compSource
    semanticError pos $ concat
        [ "Use of unimplemented language feature: ", feature, "\n"
        , showCodeAt source l c
        ]

-- run semantic analysis on an AST instance
analyzeAST :: AST -> MSemanticAnalyzer ()
analyzeAST ast = do
    logMsgLn "=== Running semantic analysis ==="
    analyzeAST ast
    return ()
    where
        -- perform semantic analysis on an AST
        analyzeAST :: AST -> MSemanticAnalyzer ()
        analyzeAST (AST _ scope) = do
            logMsgLn "Analyzing AST"
            logMsgLn "Analyzing global scope"
            pushNewScope
            prog <- analyzeScope scope
            symbols <- getTopScope
            let varTypes = List.map (toMilType . MSemantics.varType . symInfo) $ HashMap.elems vars
                vars = HashMap.filter isVar symbols
                isVar sym = case symInfo sym of
                    MVarSym _ _ _ MVariable -> True
                    _ -> False
            allocFrame <- newBasicBlock [AllocateSlots varTypes] Fallthrough
            freeFrame <- newBasicBlock [ReleaseSlots varTypes] Fallthrough
            let mainf = Function "main__" Nothing [] ([allocFrame] ++ prog ++ [freeFrame])
            pushMilFunction mainf
            logMsgLn "Generated MIL for main function:"
            logFunction mainf
            removeScope

        -- perform semantic analysis on a scope
        analyzeScope :: MScope -> MSemanticAnalyzer [BasicBlock]
        analyzeScope scope@(MScope decls stmts) = do
            logMsgLn "Collecting scope declarations"
            collectDecls decls
            logMsgLn "Analyzing statements"
            bbs <- analyzeStatements stmts
            logMsgLn "Generated MIL for current scope:"
            logBBs bbs
            return bbs

        -- perform semantic analysis on a statement
        analyzeStatements :: [WithPos MStatement] -> MSemanticAnalyzer [BasicBlock]
        analyzeStatements [] = return []
        analyzeStatements (stmt:stmts) = analyze >+> analyzeStatements stmts where
            pos = positionOf stmt
            analyze :: MSemanticAnalyzer [BasicBlock]
            analyze = case removePos stmt of
                IfThenElse expr thenStmt elseStmt -> do
                    logMsgLn "-- analyzing IfThenElse statement"
                    logTreeLines 4 stmt
                    (tt, condition) <- analyzeExpr expr
                    source <- fromEnv compSource
                    assertThat (tt == MBool) (positionOf expr) $ concat
                        [ "Condition for IfThenElse block must be of type MBool, not ", show tt, "\n"
                        , let AlexPn _ l c = positionOf expr in showCodeAt source l c
                        ]
                    thenBody <- analyzeStatements [thenStmt]
                    elseBody <- analyzeStatements [elseStmt]
                    mergePoint <- newBasicBlock [] Fallthrough
                    thenExit <- newBasicBlock [] (Jump (blockId mergePoint))
                    elseHead <- newBasicBlock [] Fallthrough
                    conditionCheck <- newBasicBlock [] (BranchZero condition (blockId elseHead))
                    return (conditionCheck:thenBody ++ [thenExit] ++ elseHead:elseBody ++ [mergePoint])
                WhileDo expr bodyStmt -> do
                    logMsgLn "-- analyzing WhileDo statement"
                    logTreeLines 4 stmt
                    (tt, condition) <- analyzeExpr expr
                    source <- fromEnv compSource
                    assertThat (tt == MBool) (positionOf expr) $ concat
                        [ "Condition for WhileDo block must be of type MBool, not ", show tt, "\n"
                        , let AlexPn _ l c = positionOf expr in showCodeAt source l c
                        ]
                    mergePoint <- newBasicBlock [] Fallthrough
                    conditionCheck <- newBasicBlock [] (BranchZero condition (blockId mergePoint))
                    loopBody <- analyzeStatements [bodyStmt]
                    jumpBack <- newBasicBlock [] (Jump (blockId conditionCheck))
                    return (conditionCheck:loopBody ++ [jumpBack, mergePoint])
                Assign loc expr -> do
                    logMsgLn "-- analyzing Assign statement"
                    logTreeLines 4 stmt
                    (vart, msym) <- analyzeLoc loc
                    symEntry <- lookupSymbol "symbol" (symbolName msym) (positionOf loc)
                    (exprt, val) <- analyzeExpr expr
                    source <- fromEnv compSource
                    assertThat (vart == exprt) (positionOf loc) $ concat
                        [ "Cannot assign value of type ", show exprt, " to variable of type ", show vart, "\n"
                        , "Expression at ", showAlexPos (positionOf expr), ":\n"
                        , let AlexPn _ l c = positionOf expr in showCodeAt source l c, "\n"
                        , "Variable assigned at ", showAlexPos (positionOf loc), "\n"
                        , let AlexPn _ l c = positionOf loc in showCodeAt source l c, "\n"
                        , "Variable declared at ", showAlexPos (declPos symEntry), "\n"
                        , let AlexPn _ l c = declPos symEntry in showCodeAt source l c, "\n"
                        ]
                    bb <- newBasicBlock [Store msym val] Fallthrough
                    return [bb]
                MRead loc -> do
                    logMsgLn "-- analyzing Read statement"
                    logTreeLines 4 stmt
                    (_, msym) <- analyzeLoc loc
                    bb <- newBasicBlock [Read msym] Fallthrough
                    return [bb]
                MPrint expr -> do
                    logMsgLn "-- analyzing Print statement"
                    logTreeLines 4 stmt
                    (t, v) <- analyzeExpr expr
                    bb <- newBasicBlock [Print (toMilType t) v] Fallthrough
                    return [bb]
                CodeBlock scope -> do
                    logMsgLn "-- analyzing Block statement"
                    logTreeLines 4 stmt
                    pushNewScope
                    bbs <- analyzeScope scope
                    symbols <- getTopScope
                    removeScope
                    let varTypes = List.map (toMilType . MSemantics.varType . symInfo) $ HashMap.elems vars
                        vars = HashMap.filter isVar symbols
                        isVar sym = case symInfo sym of
                            MVarSym _ _ _ MVariable -> True
                            _ -> False
                    allocFrame <- newBasicBlock [PushBlock, AllocateSlots varTypes] Fallthrough
                    freeFrame <- newBasicBlock [ReleaseSlots varTypes, PopBlock] Fallthrough
                    return ([allocFrame] ++ bbs ++ [freeFrame])
                MReturn expr -> do
                    logMsgLn "-- analyzing Return statement"
                    logTreeLines 4 stmt
                    (t, v) <- analyzeExpr expr

                    bb <- newBasicBlock [] (Return (Just (toMilType t, v)))
                    return [bb]
                _ -> unimplementedFeatureError (positionOf stmt) ("Unsuported statement")
            -- helper for analyzing destination of a read or assignmented
            analyzeLoc :: WithPos MLocation -> MSemanticAnalyzer (MType, Symbol)
            analyzeLoc (WithPos (MLocation name dims) pos@(AlexPn _ l c)) = do
                source <- fromEnv compSource
                -- assertDefined name pos $ concat ["No variable named ", show name, "\n", showCodeAt source l c]
                (entry, level) <- lookupWithLevel "symbol" name pos
                milSym <- case entry of
                    MSymbolEntry _ _ (MVarSym tt off ds k) -> do
                        assertThat (length dims == ds) pos $
                            concat  [ show k, show name, " has ", show ds, " dimensions, not ", show (length dims)
                                    , showCodeAt source l c
                                    ]
                        unless (List.null dims) (unimplementedFeatureError pos "Use of arrays")
                        mapM_ analyzeExpr dims
                        return (tt, StackLocal name (toMilType tt) off level)
                    MSymbolEntry _ declPos@(AlexPn _ l' c') _ -> semanticError pos $
                        concat  [ show name, " is not a variable:\n"
                                , showCodeAt source l c, "\n"
                                , "declared at ", showAlexPos declPos, ":\n"
                                , showCodeAt source l' c'
                                ]
                mapM_ analyzeExpr dims
                return milSym
            -- analyzeCases :: WithPos MExpression -> [WithPos MCase] -> MSemanticAnalyzer [[BasicBlock]]
            -- analyzeCases _ [] = return []
            -- analyzeCases expr (WithPos (MCase (MDtor name dtorargs) stmts) pos@(AlexPn _ l c):cases) = do
            --     exprT <- analyzeExpr expr
            --     entry <- lookupMaybe name
            --     source <- fromEnv compSource
            --     case entry of
            --         Just (MSymbolEntry _ _ (MCtorSym _ dtorT)) -> do
            --             assertThat (typeName exprT == dtorT) pos $
            --                 concat  ["Type of case (",typeName exprT,") and type of expression case (",dtorT,") do not match. Case is:\n"
            --                         , showCodeAt source l c, "\n"
            --                         , "Expression is:\n"
            --                         , let (AlexPn _ l c) = positionOf expr in showCodeAt source l c, "\n"
            --                         ]
            --         Just (MSymbolEntry _ declPos@(AlexPn _ decll declc) _) -> semanticError pos $
            --             concat  [show name, " is not a value constructor:\n"
            --                     , showCodeAt source l c, "\n"
            --                     , "declared at ", showAlexPos declPos, ":\n"
            --                     , showCodeAt source decll declc
            --                     ]
            --         Nothing -> semanticError pos $ concat ["No value constructor named ", show name, "\n", showCodeAt source l c]
            --     analyzeCases expr cases

        -- collect and analyze declarations
        collectDecls :: [WithPos MDeclaration] -> MSemanticAnalyzer ()
        collectDecls decls = do
            logMsgLn "Collecting type declarations"
            collectDeclsWith typeCollector decls
            logMsgLn "Collecting value constructors"
            collectDeclsWith ctorCollector decls
            logMsgLn "Collecting variables and functions"
            collectDeclsWith varsAndFunCollector decls
            logMsgLn "Collecting function implementations"
            collectDeclsWith functionCollector decls

        -- collect declarations using a specific analysis function (the collector)
        collectDeclsWith :: (MDeclaration -> MSemanticAnalyzer ()) -> [WithPos MDeclaration] -> MSemanticAnalyzer ()
        collectDeclsWith collector decls = do
            mapM_ (mapNoPos collector) decls
            logMsgLn "-- symbol table is now:"
            logSymbolTable

        -- collector for data type declarations
        typeCollector :: MDeclaration -> MSemanticAnalyzer ()
        typeCollector decl = case decl of
            Data n _ -> do
                logMsgLn "-- found data type declaration"
                let name = mapNoPos midName n
                    pos = positionOf n
                defineSymbol name pos MTypeSym
            _ -> return ()

        -- collector for value constructors
        ctorCollector :: MDeclaration -> MSemanticAnalyzer ()
        ctorCollector decl = case decl of
            Data tname ctors -> do
                let typeName = mapNoPos midName tname
                assertDefined typeName (positionOf tname) ("Expecting " ++ show typeName ++ " to have been defined but was not")
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

        -- collector for variable and function declarations
        --
        -- This collector assumes that all data type that may appear in the
        -- current scope have been collected.
        -- Note that for functions, only their type and name are collected, not their
        -- definition/implementation.
        varsAndFunCollector :: MDeclaration -> MSemanticAnalyzer ()
        varsAndFunCollector decl = case decl of
            Vars vars t -> do
                logMsgLn "-- found variable declarations"
                assertTypeDefined t
                when (mapNoPos isUserType t) (unimplementedFeatureError (positionOf t) ("User defined type " ++ mapNoPos show t))
                mapM_ visitVar vars
                where
                    visitVar :: WithPos DeclSpec -> MSemanticAnalyzer ()
                    visitVar v = do
                        logMsgLn "-- found variable"
                        let name = mapNoPos varName v
                            dims = mapNoPos (length . varDims) v
                            pos = positionOf v
                        i <- countVariables
                        unless (dims == 0) (unimplementedFeatureError pos "Array declaration")
                        defineSymbol name pos (MVarSym (removePos t) (i+1) dims MVariable)
            Fun n rt ps _ _ -> do
                logMsgLn "-- found function declaration"
                assertTypeDefined rt
                assertNotUserType rt
                mapM_ ((\t -> assertTypeDefined t >> assertNotUserType t). paramType . removePos) ps
                let name = mapNoPos midName n
                    rtype = removePos rt
                    params = Prelude.map (mapNoPos collectParams) ps
                    collectParams p = (removePos (paramType p), paramDim p)
                    pos = positionOf n
                label <- getFunctionLabel "function"
                defineSymbol name pos (MFunSym params rtype label)
            _ -> return ()
            where
                assertNotUserType t = when (mapNoPos isUserType t) (unimplementedFeatureError (positionOf t) ("User defined type " ++ mapNoPos show t))

        -- collector for function definitions/implementations
        --
        -- This collector assumes that all data type and variable that may appear
        -- in the scope of the function definitions have been collected.
        functionCollector :: MDeclaration -> MSemanticAnalyzer ()
        functionCollector decl = case decl of
            Fun n rt params decls stmts -> do
                logMsgLn "-- found function declaration"
                source <- fromEnv compSource
                let name = mapNoPos midName n
                    pos@(AlexPn _ l c) = positionOf n
                assertDefined name pos $
                    concat ["Function ", show name , " is not in the symbol table!!!"]
                Just (MSymbolEntry _ _ (MFunSym paramt rett label)) <- lookupMaybe name
                pushNewScope
                logMsgLn "Collecting function parameters"
                collectParams params
                logMsgLn "-- symbol table is now"
                logMsgLn "Collecting local function declarations"
                collectDecls decls
                logMsgLn "Anallyzing function body"
                bbs <- analyzeStatements stmts
                mapM_ checkReturnType stmts
                symbols <- getTopScope
                let varTypes = List.map (toMilType . MSemantics.varType . symInfo) $ HashMap.elems vars
                    vars = HashMap.filter isVar symbols
                    isVar sym = case symInfo sym of
                        MVarSym _ _ _ MVariable -> True
                        _ -> False
                allocFrame <- newBasicBlock [AllocateSlots varTypes] Fallthrough
                freeFrame <- newBasicBlock [ReleaseSlots varTypes] Fallthrough
                let fbody = Function label (Just (toMilType rett)) (List.map (toMilType.fst) paramt) ([allocFrame] ++ bbs ++ [freeFrame])
                pushMilFunction fbody
                logMsgLn "Generated MIL for function body:"
                logFunction fbody
                removeScope
                where
                    -- helper for collecting parameter declarations
                    collectParams :: [WithPos MParamDecl] -> MSemanticAnalyzer ()
                    collectParams [] = return ()
                    collectParams (WithPos (MParamDecl pname pdim ptype) ppos:paramDecls) = do
                        logMsgLn "-- found parameter"
                        assertTypeDefined ptype
                        i <- countParameters
                        defineSymbol pname ppos (MVarSym (removePos ptype) (i+1) pdim MParameter)
                        collectParams paramDecls
                    -- helpder for checking the return type of the function and returned values match
                    checkReturnType :: WithPos MStatement -> MSemanticAnalyzer ()
                    checkReturnType s = case removePos s of
                        MReturn expr -> do
                            (t, _) <- analyzeExpr expr
                            source <- fromEnv compSource
                            assertThat (removePos rt == t) (positionOf expr) $ concat
                                [ "Return expression is of type ", show t, " but function return type is ", mapNoPos show rt, "\n"
                                , let (AlexPn _ l c) = positionOf expr in showCodeAt source l c, "\n"
                                , "function return type defined at ", showAlexPos (positionOf rt), "\n"
                                , let (AlexPn _ l c) = positionOf rt in showCodeAt source l c
                                ]
                        CodeBlock (MScope _ ss) -> mapM_ checkReturnType ss
                        _ -> return ()
            _ -> return ()

        -- perfrom semantic analysis on an expression
        --
        -- This function assumes all symbols that may be used in the current scope
        -- are defined in the symbol tabel.
        analyzeExpr :: WithPos MExpression -> MSemanticAnalyzer (MType, MilValue)
        analyzeExpr expr = let pos@(AlexPn _ opl opc) = positionOf expr in logMsgLn "-- Analyzing sub-expression" >> case removePos expr of
            MBinaryOp op lhs rhs -> do
                logMsgLn "-- analyzing binary operation"
                logTreeLines 4 expr
                (ltype, lval) <- analyzeExpr lhs
                (rtype, rval) <- analyzeExpr rhs
                source <- fromEnv compSource
                let lhsPos@(AlexPn _ lhsl lhsc) = positionOf lhs
                    rhsPos@(AlexPn _ rhsl rhsc) = positionOf rhs
                assertThat (ltype == rtype) pos $
                    concat  [ "Operands of ", show op, " must have the same type:\n"
                            , showCodeAt source opl opc, "\n"
                            , "LHS has type ", show ltype, "\n"
                            , showCodeAt source lhsl lhsc, "\n"
                            , "RHS has type ", show rtype, "\n"
                            , showCodeAt source rhsl rhsc
                            ]
                let toMilOp o = case o of
                        MAdd -> AddOp
                        MSub -> SubOp
                        MMul -> MulOp
                        MDiv -> DivOp
                        MEqual -> EQOp
                        MLessThan -> LTOp
                        MLessEqual -> LEOp
                        MGreaterThan -> GTOp
                        MGreaterEqual -> GEOp
                        MAnd -> AndOp
                        MOr -> OrOp
                if op `elem` [MAdd, MSub, MMul, MDiv]
                    then do
                        assertThat (ltype == MInt || ltype == MReal) pos $ concat
                            [ "Operation ", show op, " can only be done on values of type MInt and MReal, not ", show ltype, "\n"
                            , showCodeAt source opl opc
                            ]
                        return (ltype, BinaryOp (toMilType ltype) (toMilOp op) lval rval)
                    else if op `elem` [MEqual, MLessThan, MLessEqual, MGreaterThan, MGreaterEqual]
                        then do
                            assertThat (ltype == MInt || ltype == MReal || ltype == MChar) pos $ concat
                                [ "Operation ", show op, " can only be done on values of type MInt and MReal, not ", show ltype, "\n"
                                , showCodeAt source opl opc
                                ]
                            return (MBool, BinaryOp Bool (toMilOp op) lval rval)
                        else if op `elem` [MAnd, MOr]
                            then do
                                assertThat (ltype == MBool) pos $ concat
                                    [ "Operation ", show op, " can only be done on values of type Bool, not ", show ltype, "\n"
                                    , showCodeAt source opl opc
                                    ]
                                return (MBool, BinaryOp Bool (toMilOp op) lval rval)
                            else unimplementedFeatureError pos ("Unrecognized operation " ++ show op)
            MUnaryOp op sube -> do
                logMsgLn "-- analyzing unary operation"
                logTreeLines 3 expr
                (subType, subVal) <- analyzeExpr sube
                source <- fromEnv compSource
                let subpos@(AlexPn _ subl subc) = positionOf sube
                    assertTypeIn ts = assertThat (subType `elem` ts) subpos $
                        concat  [ "Operand type of ", show op, " must be one of ", show ts, ", not ", show subType, "\n"
                                , showCodeAt source subl subc
                                ]
                case op of
                    MNeg -> assertTypeIn [MInt, MReal] >> return (subType, UnaryOp (toMilType subType) NegativeOp subVal)
                    MNot -> assertTypeIn [MBool] >> return (MBool, UnaryOp Bool BooleanNotOp subVal)
                    MFloat -> assertTypeIn [MInt] >> return (MReal, UnaryOp F32 FloatOp subVal)
                    MFloor -> assertTypeIn [MReal] >> return (MInt, UnaryOp I32 FloorOp subVal)
                    MCeil -> assertTypeIn [MReal] >> return (MInt, UnaryOp I32 CeilingOp subVal)
            -- MSize (MIdName name) dims -> do
            --     entry <- lookupMaybe name
            --     source <- fromEnv compSource
            --     case entry of
            --         Just (MSymbolEntry _ _ (MVarSym _ ds)) -> do
            --             assertThat (dims == ds) pos $
            --                 concat  [ "Variable ", show name, " has ", show ds, " dimensions, not ", show dims
            --                         , showCodeAt source opl opc, "\n"
            --                         , "Did you mean `", name, concat (take ds (repeat "[]")) ,"`"
            --                         ]
            --             return MInt
            --         Just (MSymbolEntry _ declPos@(AlexPn _ l c) _) -> semanticError pos $
            --             concat  [show name, " is not a variable:\n"
            --                     , showCodeAt source opl opc, "\n"
            --                     , "declared at ", showAlexPos declPos, ":\n"
            --                     , showCodeAt source l c
            --                     ]
            --         Nothing -> semanticError pos $ concat ["No variable named ", show name, "\n", showCodeAt source opl opc]
            MCall (MIdName name) args -> do
                logMsgLn "-- analyzing call operation"
                logTreeLines 4 expr
                entry <- lookupWithLevelMaybe name
                source <- fromEnv compSource
                case entry of
                    Just (MSymbolEntry _ declPos@(AlexPn _ decll declc) (MFunSym paramt rt label), level) -> do
                        assertThat (length args == length paramt) pos $ concat
                            [ "Function ", show name, " expects ", show (length paramt)
                            , " arguments but ", show (length args), " were given\n"
                            , showCodeAt source opl opc
                            ]
                        args' <- mapM analyzeExpr args
                        assertArgsMatch 0 args' paramt
                        return (rt, Call (FunctionLabel label (Just (toMilType rt)) (List.map (toMilType.fst) paramt) level) (List.map snd args'))
                        where
                            assertArgsMatch :: Int -> [(MType, MilValue)] -> [(MType, Int)] -> MSemanticAnalyzer ()
                            assertArgsMatch _ [] [] = return ()
                            assertArgsMatch i ((argT,_):argTs) ((paramT, _):params) = do
                                let argPos@(AlexPn _ argl argc) = positionOf (args !! i)
                                    pos@(AlexPn _ l c) = positionOf expr
                                assertThat (argT == paramT) argPos $ concat
                                    [ "Argument ", show i, " has type ", show argT
                                    , " but function ", show name, " expects ", show paramT, ":\n"
                                    , showCodeAt source argl argc, "\n"
                                    , "function call at ", showAlexPos pos,":\n"
                                    , showCodeAt source l c, "\n"
                                    , "function declared at ", showAlexPos declPos,":\n"
                                    , showCodeAt source decll declc, "\n"
                                    ]
                                assertArgsMatch (i+1) argTs params
                    Just (MSymbolEntry _ declPos@(AlexPn _ l c) _, _) -> semanticError pos $
                        concat  [show name, " is not a function:\n"
                                , showCodeAt source opl opc, "\n"
                                , "declared at ", showAlexPos declPos, ":\n"
                                , showCodeAt source l c
                                ]
                    Nothing -> semanticError pos $ concat ["No function named ", show name, "\n", showCodeAt source opl opc]
            MVar (MIdName name) dims -> do
                logMsgLn "-- analyzing variable use"
                logTreeLines 4 expr
                (entry, level) <- lookupWithLevel "symbol" name pos
                source <- fromEnv compSource
                case entry of
                    MSymbolEntry _ _ (MVarSym tt off ds k) -> do
                        assertThat (length dims == ds) pos $
                            concat  [ show k, show name, " has ", show ds, " dimensions, not ", show (length dims)
                                    , showCodeAt source opl opc
                                    ]
                        mapM_ analyzeExpr dims
                        return (tt, Load (StackLocal name (toMilType tt) off level))
                    MSymbolEntry _ declPos@(AlexPn _ l c) _ -> semanticError pos $
                        concat  [ show name, " is not a variable:\n"
                                , showCodeAt source opl opc, "\n"
                                , "declared at ", showAlexPos declPos, ":\n"
                                , showCodeAt source l c
                                ]
            MConst c -> do
                logMsgLn "-- found literal constant"
                logTree expr
                case c of
                    IntConst val -> return (MInt, ConstI32 val)
                    RealConst val -> return (MReal, ConstF32 val)
                    CharConst val -> return (MChar, ConstChar val)
                    BoolConst val -> return (MBool, ConstBool val)
            _ -> unimplementedFeatureError pos ("Unsuported expression")

        -- helper for removing a scope from the symbol table and logging the change
        removeScope :: MSemanticAnalyzer ()
        removeScope = do
            logMsgLn "-- poping scope from symbol table"
            popScope
            logMsgLn "-- symbol table is now:"
            logSymbolTable

        -- helper for translating an MType into a MIL type
        toMilType :: MType -> MilType
        toMilType t = case t of
            MInt -> I32
            MReal -> F32
            MChar -> Char
            MBool -> Bool
            _ -> undefined

        -- helpder for identifying user defined types
        isUserType :: MType -> Bool
        isUserType (MUserType _) = True
        isUserType _ = False

-- run semantic analysis in a compiler monad instance
runSemanticAnalyzer :: Monad m => (AST -> MSemanticAnalyzer a) -> AST -> MSemanticAnalyzerEnvironment -> CompilerMonadT (a, MSemanticAnalyzerState) m
runSemanticAnalyzer analyzer ast env = do
    let (c, s) = runState (runCompilerT . analyzer $ ast) (initSemanticAnalyzer env)
    a <- compiler c
    return (a, s)
