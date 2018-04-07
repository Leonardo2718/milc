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
                _ -> [ basicBlockMerging
                     , forwardAnd (logMsgLn "%%% Performing No Opt %%%")
                     -- , localValueSimplification
                     ]
    functions' <- mapM (optimizeUsing optimizations) functions
    logMsgLn "=== All optimization complete ==="
    return (Mil functions')

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

{-
-- state for local copy propagation
--
-- The `copyTable` field is a map of symbol names to MIL values. The table keeps
-- track of symbols that are simple copies of either a variable or a constant.
-- This table is used for copy propagation.
data LocalValueSimplificationState = LocalValueSimplificationState { copyTable :: HashMap Symbol MilValue }
type LocalValueSimplificationStateMonad = State LocalValueSimplificationState
type LocalValueSimplification a = CompilerMonadT a LocalValueSimplificationStateMonad

-- propagate copies and fold constant values in the same pass
--
-- This optimization essentially performs three optimizations in one pass:
--  * local copy propagation
--  * constant folding
--  * basic strength reduction
localValueSimplification :: Monad m => Function -> CompilerMonadT Function m
localValueSimplification mil = do
    let (a, s) = runState (runCompilerT (doValueSimplification mil)) (LocalValueSimplificationState HashMap.empty)
    mil' <- compiler a
    return mil'
    where
        -- just simplify values in the contianed basic blocks
        doValueSimplification :: Function -> LocalValueSimplification Function
        doValueSimplification mil@(Function _ bbs) = do
            logMsgLn "%%%% Performing: Local Value Simplification %%%%"
            bbs' <- mapM (\bb -> simplifyInBlock bb =>> resetCopyTable) bbs
            return $ Mil bbs'

        -- just simplify values in the contained opcodes and the terminator
        simplifyInBlock :: BasicBlock -> LocalValueSimplification BasicBlock
        simplifyInBlock (BasicBlock bbid opcodes t) = do
            logMsgLn ("-- simplifying values in basic block " ++ show bbid)
            opcodes' <- simplifyInOpCodes opcodes
            t' <- simplifyInTerminator t
            return $ BasicBlock bbid opcodes' t'

        -- recursively simplify values in each opcode in a list
        simplifyInOpCodes :: [OpCode] -> LocalValueSimplification [OpCode]
        simplifyInOpCodes (op:ops) = do
            logMsgLn ("-- simplifying values in opcode: " ++ show op)
            op' <- case op of
                Store sym val@(Const _) -> do
                    -- if the opcode is a store of the constant
                    -- record the symbol as a copy of the constant
                    recordCopy sym val
                    return op
                Store sym val@(Load _) -> do
                    -- if the opcode is a load, record the symbol as a copy of
                    -- the loaded variable
                    val' <- propagateCopies val
                    recordCopy sym val'
                    return op
                Store sym val -> do
                    -- if the opcode is a store (not handled above), simplify
                    -- the value being stored, and if the result is a constant
                    -- or a load, record the symbol as a copy
                    val' <- simplifyValue val
                    case val' of
                        Const _ -> recordCopy sym val'
                        Load  _ -> recordCopy sym val'
                        _ -> killCopy sym
                    return $ Store sym val'
                Print val -> do
                    -- if the opcode is a print, simplify the value being
                    -- printed
                    val' <- simplifyValue val
                    return $ Print val'
                Read sym -> do
                    -- if the opcode is a read, remove (kill) the symbol being
                    -- stored to from the copyTable
                    killCopy sym
                    return op
                _ -> return op
            logMsgLn ("-- opcode after value simplification: " ++ show op')
            ops' <- simplifyInOpCodes ops
            return $ op':ops'
        simplifyInOpCodes [] = return []

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
                BinaryOp op lhs rhs -> do
                    lhs' <- propagateCopies lhs
                    rhs' <- propagateCopies rhs
                    return $ BinaryOp op lhs' rhs'
                Load sym -> do
                    v <- tableLookup sym
                    case v of
                        Just v' -> return v'
                        Nothing -> return val
                _ -> return val

        -- recursively fold a values sub-expressions and then the resulting
        -- expression itself
        foldValueRecursively :: MilValue -> LocalValueSimplification MilValue
        foldValueRecursively val@(BinaryOp op lhs rhs) = do
            logMsgLn ("Folding " ++ show val)
            lhs' <- foldValueRecursively lhs
            rhs' <- foldValueRecursively rhs
            foldValue $ BinaryOp op lhs' rhs'
        foldValueRecursively val = return val

        -- fold a MilValue
        --
        -- In this implementation, "folding" means both constant folding and
        -- reduction in strength. Examples of the transformation applied by this
        -- optimization are:
        --
        --  x + 0 => a
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
                BinaryOp AddOp (Const 0) rhs -> return rhs
                BinaryOp AddOp lhs (Const 0) -> return lhs
                BinaryOp SubOp lhs (Const 0) -> return lhs
                BinaryOp MulOp (Const 1) rhs -> return rhs
                BinaryOp MulOp lhs (Const 1) -> return lhs
                BinaryOp DivOp lhs (Const 1) -> return lhs
                BinaryOp DivOp _ (Const 0) -> return val -- don't fold division by 0
                BinaryOp MulOp (Const 0) _ -> return $ Const 0
                BinaryOp MulOp _ (Const 0) -> return $ Const 0
                BinaryOp DivOp (Const 0) _ -> return $ Const 0
                BinaryOp AddOp (Load a) (Load b) -> if a == b
                                                    then return (BinaryOp MulOp (Const 2) (Load a))
                                                    else return val
                BinaryOp SubOp (Load a) (Load b) -> if a == b
                                                    then return (Const 0)
                                                    else return val
                BinaryOp AddOp (Const a) (Const b) -> return $ Const (a + b)
                BinaryOp SubOp (Const a) (Const b) -> return $ Const (a - b)
                BinaryOp MulOp (Const a) (Const b) -> return $ Const (a * b)
                BinaryOp DivOp (Const a) (Const b) -> return $ Const (a `div` b)
                _ -> return val
            logMsgLn (concat2WithPadding 12 "   into:" (show val'))
            return val'

        -- record a symbol as a copy of a value
        recordCopy :: Symbol -> MilValue -> LocalValueSimplification ()
        recordCopy sym val = do
            logMsgLn $ concat ["-- adding ", sym, " as a copy of ", show val]
            s <- get
            let t = copyTable s
                t' = HashMap.insert sym val t
            put s{copyTable=t'}

        -- remove a symbol from the copyTable
        killCopy :: Symbol -> LocalValueSimplification ()
        killCopy sym = do
            logMsgLn ("-- killing symbol " ++ sym ++ " as a copy")
            s <- get
            let t = copyTable s
                t' = HashMap.delete sym t
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
                v = HashMap.lookup sym t
            return v
-}
