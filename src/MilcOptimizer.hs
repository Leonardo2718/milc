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

{- this module implements the milc optimization infrastructure -}

module MilcOptimizer where

import CompilerEnvironment
import MilcUtils
import MIL
import MilcCFG

import Data.HashMap.Strict as HashMap
import Data.List as List
import Control.Monad.State
import Control.Monad


-- type for storing optimization environment information
data OptimizerEnvironment = OptimizerEnvironment
    { optLevel :: Int
    }

-- run various optimizations
optimize :: Monad m => OptimizerEnvironment -> Mil -> CompilerMonadT Mil m
optimize env mil@(Mil functions) = do
    logMsgLn $ "=== Running Optimizations (level " ++ show (optLevel env) ++ ") ==="
    let
        optimizeUsing :: Monad m => [Function -> CompilerMonadT Function m] -> Function -> CompilerMonadT Function m
        optimizeUsing [] f = return f
        optimizeUsing opts f@(Function label _ _ _) = do
            logMsgLn $ concat ["%%%%% Optimizing ", show label, " %%%%%"]
            logFunction f
            f' <- foldl (>>=) (return f) (intersperse (\ f -> logFunction f >> return f) opts)
            logMsgLn $ concat ["%%%%% Optimization for ", show label, " complete %%%%%"]
            logFunction f'
            return f'
        optimizations :: Monad m => [Function -> CompilerMonadT Function m]
        optimizations = case optLevel env of
                0 -> [
                     ]
                1 -> [ basicBlockMerging
                     ]
                2 -> [ basicBlockMerging
                     , localValueSimplification
                     ]
                l -> [ forwardAnd (logError (concat ["No optimization level ", show l, " implemented.\nUse `--help` option for usage info."])) ]
    functions' <- mapM (optimizeUsing optimizations) functions
    logMsgLn "=== All optimization complete ==="
    return (Mil functions')

-- help message for the optimizer
helpMsg = intercalate "\n\n"
    [ "Optimizations"
    , optionDesc "-O0"  [ "Performs no optimizations. This may make the generated code"
                        , "more difficult to read."
                        ]
    , optionDesc "-O1 (default)"    [ "Performs minimal optizations to clean up the generated code."
                                    , "Currently, this otpimization level only performs basic block"
                                    , "merging. This can significantly reduces the number of"
                                    , "labels generated."
                                    ]
    , optionDesc "-O2"  [ "Performs a few basic optimizations. In addition to basic block merging,"
                        , "this optimization level also performs constant folding, arithmetic"
                        , "simplification, and basic strength reduction. These help to further"
                        , "clean up the code and can potentially result in performance"
                        , "improvements by reducing the total number of instructions."
                        ]
    ]

-- merge all basic blocks that can be safely merged
--
-- This optimization merges sequences of basic blocks that are safe to merge.
-- For example, in the following control flow graph:
--
--       /--->G--->\
--  A-->B-->C-->D-->E-->F
--
-- blocks C and D would be merged together, giving:
--
--       /--->G--->\
--  A-->B---->CD--->E-->F
--
--
basicBlockMerging :: Monad m => Function -> CompilerMonadT Function m
basicBlockMerging f@(Function label rt paramt bbs) = do
    logMsgLn "%%% Performing: Basic Block Merging %%%"
    logMsgLn "-- building CFG"
    cfg <- buildCFG f
    logCFG cfg
    bbs <- mergeBlocks cfg $ functionBody f
    let f' = f{functionBody=bbs}
    logMsgLn "-- after merging, CFG is now:"
    cfg' <- buildCFG f'
    logCFG cfg'
    logMsgLn "%%% Basic Block Merging complete %%%"
    return f'
    where
        mergeBlocks :: Monad m => CFG -> [BasicBlock] -> CompilerMonadT [BasicBlock] m
        mergeBlocks _ [bb] = return [bb]
        mergeBlocks cfg (bb1:bbs) = do
            bb2:bbs' <- mergeBlocks cfg bbs
            safeMergePossible <- canMergeSafely bb1 bb2 cfg
            case safeMergePossible of
                True -> do
                    logMsgLn "-- merging basic blocks"
                    bb1outs <- outgoingEdgesOf bb1 cfg
                    logMsgLn ("   " ++ concat2WithPadding 3 (show (blockId bb1)) (" out " ++ show bb1outs))
                    bb2ins <- incomingEdgesOf bb2 cfg
                    logMsgLn ("   " ++ concat2WithPadding 3 (show (blockId bb2)) (" in  " ++ show bb2ins))
                    return $ mergeBasicBlocks bb1 bb2 : bbs'
                False -> return $ bb1:bb2:bbs'


-- state for local copy propagation
--
-- The `copyTable` field is a map of symbol names to MIL values. The table keeps
-- track of symbols that are simple copies of either a variable or a constant.
-- This table is used for copy propagation.
data LocalValueSimplificationState = LocalValueSimplificationState { copyTable :: HashMap String MilValue }
type LocalValueSimplificationStateMonad = State LocalValueSimplificationState
type LocalValueSimplification a = CompilerMonadT a LocalValueSimplificationStateMonad

-- propagate copies and fold constant values in the same pass
--
-- This optimization essentially performs three optimizations in one pass:
--  * local copy propagation
--  * constant folding
--  * basic strength reduction
localValueSimplification :: Monad m => Function -> CompilerMonadT Function m
localValueSimplification f = do
    let (a, s) = runState (runCompilerT (doValueSimplification f)) (LocalValueSimplificationState HashMap.empty)
    f' <- compiler a
    return f'
    where
        -- just simplify values in the contianed basic blocks
        doValueSimplification :: Function -> LocalValueSimplification Function
        doValueSimplification f = do
            logMsgLn "%%%% Performing: Local Value Simplification %%%%"
            bbs' <- mapM (\bb -> simplifyInBlock bb =>> resetCopyTable) (functionBody f)
            return f{functionBody=bbs'}

        -- just simplify values in the contained opcodes and the terminator
        simplifyInBlock :: BasicBlock -> LocalValueSimplification BasicBlock
        simplifyInBlock (BasicBlock bbid opcodes t) = do
            logMsgLn ("-- simplifying values in basic block " ++ show bbid)
            opcodes' <- mapM simplifyInOpCode opcodes
            t' <- simplifyInTerminator t
            return $ BasicBlock bbid opcodes' t'

        -- recursively simplify values in each opcode in a list
        simplifyInOpCode :: OpCode -> LocalValueSimplification OpCode
        simplifyInOpCode op = do
            logMsgLn ("-- simplifying values in opcode: " ++ show op)
            op' <- case op of
                -- Store sym val@(Const _) -> do
                --     -- if the opcode is a store of the constant
                --     -- record the symbol as a copy of the constant
                --     recordCopy sym val
                --     return op
                -- Store sym val@(Load _) -> do
                --     -- if the opcode is a load, record the symbol as a copy of
                --     -- the loaded variable
                --     val' <- propagateCopies val
                --     recordCopy sym val'
                --     return op
                Store sym val -> do
                    -- if the opcode is a store (not handled above), simplify
                    -- the value being stored, and if the result is a constant
                    -- or a load, record the symbol as a copy
                    val' <- simplifyValue val
                    case val' of
                        -- Const _ -> recordCopy sym val'
                        -- Load  _ -> recordCopy sym val'
                        _ -> killCopy sym
                    return $ Store sym val'
                Print tt val -> do
                    -- if the opcode is a print, simplify the value being
                    -- printed
                    val' <- simplifyValue val
                    return $ Print tt val'
                Read sym -> do
                    -- if the opcode is a read, remove (kill) the symbol being
                    -- stored to from the copyTable
                    killCopy sym
                    return op
                _ -> return op
            logMsgLn ("-- opcode after value simplification: " ++ show op')
            return op'

        -- if a terminator is a branch, simplify the branch condition
        simplifyInTerminator :: Terminator -> LocalValueSimplification Terminator
        simplifyInTerminator t = case t of
            BranchZero val target -> do
                logMsgLn ("-- simplifying value in terminator: " ++ show t)
                val' <- simplifyValue val
                return $ BranchZero val' target
            Branch val target -> do
                logMsgLn ("-- simplifying value in terminator: " ++ show t)
                val' <- simplifyValue val
                return $ BranchZero val' target
            _ -> return t

        -- simplify an MilValue
        --
        -- A value is said to be "simplified" when
        --  * copies have been propagated
        --  * expressions involving only constants have been folded
        --  * complex expressions are replaced by simpler one (strength reduced)
        simplifyValue :: MilValue -> LocalValueSimplification MilValue
        simplifyValue val = propagateCopies val >>= foldValueRecursively

        -- propagate copies
        --
        -- If a variable in an expression is simply the copy of a constant
        -- or another variable, it will be replaced by the copied value.
        -- For example, in:
        --
        --      a := 3
        --      b := a + 1
        --
        -- a is a copy of 3. So after this optimization (local copy propagation)
        -- the above code will become:
        --
        --      a := 3
        --      b := 3 + 1
        --
        propagateCopies :: MilValue -> LocalValueSimplification MilValue
        propagateCopies val = do
            logMsgLn ("-- propagating copies in: " ++ show val)
            case val of
                -- BinaryOp op lhs rhs -> do
                --     lhs' <- propagateCopies lhs
                --     rhs' <- propagateCopies rhs
                --     return $ BinaryOp op lhs' rhs'
                -- Load sym -> do
                --     v <- tableLookup sym
                --     case v of
                --         Just v' -> return v'
                --         Nothing -> return val
                _ -> return val

        -- recursively fold a values sub-expressions and then the resulting
        -- expression itself
        foldValueRecursively :: MilValue -> LocalValueSimplification MilValue
        foldValueRecursively val@(BinaryOp tt op lhs rhs) = do
            logMsgLn ("Folding " ++ show val)
            lhs' <- foldValueRecursively lhs
            rhs' <- foldValueRecursively rhs
            foldValue $ BinaryOp tt op lhs' rhs'
        foldValueRecursively val = return val

        -- fold a MilValue
        --
        -- In this implementation, "folding" means both constant folding and
        -- reduction in strength. Examples of the transformation applied by this
        -- optimization are:
        --
        --  x + 0 => x
        --  x - 0 => x
        --  x * 1 => x
        --  x / 1 => x
        --  x * 0 => 0
        --  x + x => 2 * x
        --  x - x => 0
        --  1 + 2 => 3
        --  3 / 1 => 3  (note that division by 0 is not folded)
        --
        foldValue :: MilValue -> LocalValueSimplification MilValue
        foldValue val = do
            logMsgLn (concat2WithPadding 12 "-- folding:" (show val))
            val' <- case val of
                UnaryOp I32 NegativeOp (ConstI32 val) -> return $ ConstI32 (-val)
                UnaryOp F32 NegativeOp (ConstF32 val) -> return $ ConstF32 (-val)

                UnaryOp Bool BooleanNotOp (ConstBool val) -> return $ ConstBool (not val)
                UnaryOp F32 FloatOp (ConstI32 val) -> return $ ConstF32 (fromIntegral val)
                UnaryOp I32 FloorOp (ConstF32 val) -> return $ ConstI32 (floor val)
                UnaryOp I32 CeilingOp (ConstF32 val) -> return $ ConstI32 (ceiling val)

                BinaryOp I32 AddOp (ConstI32 0) rhs -> return rhs
                BinaryOp I32 AddOp lhs (ConstI32 0) -> return lhs
                BinaryOp F32 AddOp (ConstF32 0.0) rhs -> return rhs
                BinaryOp F32 AddOp lhs (ConstF32 0.0) -> return lhs
                BinaryOp I32 SubOp lhs (ConstI32 0) -> return lhs
                BinaryOp F32 SubOp lhs (ConstF32 0.0) -> return lhs

                BinaryOp I32 MulOp (ConstI32 1) rhs -> return rhs
                BinaryOp I32 MulOp lhs (ConstI32 1) -> return lhs
                BinaryOp F32 MulOp (ConstF32 1.0) rhs -> return rhs
                BinaryOp F32 MulOp lhs (ConstF32 1.0) -> return lhs
                BinaryOp I32 DivOp lhs (ConstI32 1) -> return lhs
                BinaryOp I32 DivOp _ (ConstI32 0) -> return val -- don't fold division by 0
                BinaryOp F32 DivOp lhs (ConstF32 1.0) -> return lhs
                BinaryOp F32 DivOp _ (ConstF32 0.0) -> return val -- don't fold division by 0

                BinaryOp I32 MulOp (ConstI32 0) _ -> return $ ConstI32 0
                BinaryOp I32 MulOp _ (ConstI32 0) -> return $ ConstI32 0
                BinaryOp I32 DivOp (ConstI32 0) _ -> return $ ConstI32 0
                BinaryOp F32 MulOp (ConstF32 0) _ -> return $ ConstF32 0
                BinaryOp F32 MulOp _ (ConstF32 0) -> return $ ConstF32 0
                BinaryOp F32 DivOp (ConstF32 0) _ -> return $ ConstF32 0

                BinaryOp I32 AddOp (Load a) (Load b) -> if a == b
                                                    then return (BinaryOp I32 MulOp (ConstI32 2) (Load a))
                                                    else return val
                BinaryOp F32 AddOp (Load a) (Load b) -> if a == b
                                                    then return (BinaryOp F32 MulOp (ConstF32 2.0) (Load a))
                                                    else return val
                BinaryOp I32 SubOp (Load a) (Load b) -> if a == b
                                                    then return (ConstI32 0)
                                                    else return val
                BinaryOp F32 SubOp (Load a) (Load b) -> if a == b
                                                    then return (ConstF32 0.0)
                                                    else return val

                BinaryOp I32 op (ConstI32 a) (ConstI32 b) -> return $ case op of
                    AddOp -> ConstI32 (a + b)
                    SubOp -> ConstI32 (a - b)
                    MulOp -> ConstI32 (a * b)
                    DivOp -> ConstI32 (a `div` b)
                    EQOp -> ConstBool (a == b)
                    LTOp -> ConstBool (a < b)
                    LEOp -> ConstBool (a <= b)
                    GTOp -> ConstBool (a > b)
                    GEOp -> ConstBool (a >= b)
                BinaryOp F32 op (ConstF32 a) (ConstF32 b) -> return $ case op of
                    AddOp -> ConstF32 (a + b)
                    SubOp -> ConstF32 (a - b)
                    MulOp -> ConstF32 (a * b)
                    DivOp -> ConstF32 (a / b)
                    EQOp -> ConstBool (a == b)
                    LTOp -> ConstBool (a < b)
                    LEOp -> ConstBool (a <= b)
                    GTOp -> ConstBool (a > b)
                    GEOp -> ConstBool (a >= b)
                BinaryOp Bool op (ConstBool a) (ConstBool b) -> return . ConstBool $ case op of
                    AndOp -> a && b
                    OrOp -> a || b

                BinaryOp I32 SubOp (ConstI32 0) rhs -> return $ UnaryOp I32 NegativeOp rhs
                BinaryOp F32 SubOp (ConstF32 0) rhs -> return $ UnaryOp F32 NegativeOp rhs
                _ -> return val
            logMsgLn (concat2WithPadding 12 "   into:" (show val'))
            return val'

        -- record a symbol as a copy of a value
        recordCopy :: Symbol -> MilValue -> LocalValueSimplification ()
        recordCopy sym val = do
            logMsgLn $ concat ["-- adding ", show (symbolName sym), " as a copy of ", show val]
            s <- get
            let t = copyTable s
                t' = HashMap.insert (symbolName sym) val t
            put s{copyTable=t'}

        -- remove a symbol from the copyTable
        killCopy :: Symbol -> LocalValueSimplification ()
        killCopy sym = do
            logMsgLn ("-- killing symbol " ++ show (symbolName sym) ++ " as a copy")
            s <- get
            let t = copyTable s
                t' = HashMap.delete (symbolName sym) t
            put s{copyTable=t'}

        -- remove all recorded copies from the table
        resetCopyTable :: LocalValueSimplification ()
        resetCopyTable = do
            logMsgLn "-- resetting copy table"
            s <- get
            put s{copyTable=empty}

        -- check if a symbol is in the copy table
        tableLookup :: Symbol -> LocalValueSimplification (Maybe MilValue)
        tableLookup sym = do
            s <- get
            let t = copyTable s
                v = HashMap.lookup (symbolName sym) t
            return v
