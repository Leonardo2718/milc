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
import Control.Monad.State

-- run various optimizations
optimize :: Monad m => Mil -> CompilerMonadT Mil m
optimize mil = do
    logMsgLn "=== Running Optimizations ==="
    basicBlockMerging mil >>= constantFolding
                          >>= localCopyPropagation
                          >>= constantFolding

-- merge all basic blocks that can be safely merged
basicBlockMerging :: Monad m => Mil -> CompilerMonadT Mil m
basicBlockMerging mil@(Mil bbs) = do
    logMsgLn "Performing: Basic Block Merging"
    logMsgLn "-- building CFG"
    cfg <- buildCFG mil
    logCFG cfg
    bbs' <- mergeBlocks cfg bbs
    logMsgLn "-- resulting MIL"
    logMil bbs'
    return (Mil bbs')
    where
        mergeBlocks :: Monad m => CFG -> [BasicBlock] -> CompilerMonadT [BasicBlock] m
        mergeBlocks cfg (bb1:bb2:bbs) = do
            safeMergePossible <- canMergeSafely bb1 bb2 cfg
            case safeMergePossible of
                True -> do
                    logMsgLn "-- merging basic blocks"
                    bb1outs <- outgoingEdgesOf bb1 cfg
                    logMsgLn ("   " ++ concat2WithPadding 3 (show (blockId bb1)) (" out " ++ show bb1outs))
                    bb2ins <- incomingEdgesOf bb2 cfg
                    logMsgLn ("   " ++ concat2WithPadding 3 (show (blockId bb2)) (" in  " ++ show bb2ins))
                    mergeBlocks cfg (mergeBasicBlocks bb1 bb2:bbs)
                False -> do
                    bbs' <- mergeBlocks cfg (bb2:bbs)
                    return (bb1:bbs')
        mergeBlocks _ bbs = return bbs

-- fold sub-expressions that only involve constants
constantFolding :: Monad m => Mil -> CompilerMonadT Mil m
constantFolding mil@(Mil bbs) = do
    logMsgLn "Performing: Constant Folding"
    bbs' <- mapM foldInBlock bbs
    return (Mil bbs')
    where
        foldInBlock bb@(BasicBlock bbid opcodes terminator) = do
            opcodes' <- mapM foldInOpcode opcodes
            terminator' <- foldInTerminator terminator
            return (BasicBlock bbid opcodes' terminator')
        foldInOpcode (Print val) = do
            val' <- foldRecursiveValue val
            return (Print val')
        foldInOpcode (Store name val) = do
            val' <- foldRecursiveValue val
            return (Store name val')
        foldInOpcode opcode = return opcode
        foldInTerminator (Branch val target) = do
            val' <- foldRecursiveValue val
            return (Branch val' target)
        foldInTerminator (BranchZero val target) = do
            val' <- foldRecursiveValue val
            return (BranchZero val' target)
        foldInTerminator (Return (Just val)) = do
            val' <- foldRecursiveValue val
            return (Return (Just val'))
        foldInTerminator terminator = return terminator
        foldRecursiveValue val@(BinaryOp op lhs rhs) = do
            logMsgLn ("Folding " ++ show val)
            lhs' <- foldRecursiveValue lhs
            rhs' <- foldRecursiveValue rhs
            foldValue $ BinaryOp op lhs' rhs'
        foldRecursiveValue val = return val
        foldValue val = do
            logMsgLn (concat2WithPadding 12 "-- folding:" (show val))
            val' <- case val of
                BinaryOp AddOp (Const 0) rhs -> return rhs
                BinaryOp AddOp lhs (Const 0) -> return lhs
                BinaryOp SubOp lhs (Const 0) -> return lhs
                BinaryOp MulOp (Const 1) rhs -> return rhs
                BinaryOp MulOp lhs (Const 1) -> return lhs
                BinaryOp DivOp lhs (Const 1) -> return lhs
                BinaryOp DivOp _ (Const 0) -> return val
                BinaryOp MulOp (Const 0) _ -> return $ Const 0
                BinaryOp MulOp _ (Const 0) -> return $ Const 0
                BinaryOp DivOp (Const 0) _ -> return $ Const 0
                BinaryOp AddOp (Const a) (Const b) -> return $ Const (a + b)
                BinaryOp SubOp (Const a) (Const b) -> return $ Const (a - b)
                BinaryOp MulOp (Const a) (Const b) -> return $ Const (a * b)
                BinaryOp DivOp (Const a) (Const b) -> return $ Const (a `div` b)
                _ -> return val
            logMsgLn (concat2WithPadding 12 "-- into:" (show val'))
            return val'

-- state for local copy propagation
data LocalCopyPropagationState = LocalCopyPropagationState { copyTable :: HashMap Symbol MilValue }
type LocalCopyPropagationStateMonad = State LocalCopyPropagationState
type LocalCopyPropagation a = CompilerMonadT a LocalCopyPropagationStateMonad

-- execute local copy propagation and unwrap the state monad
localCopyPropagation :: Monad m => Mil -> CompilerMonadT Mil m
localCopyPropagation mil = do
    let (a, s) = runState (runCompilerT (doLocalCopyPropagaion mil)) (LocalCopyPropagationState HashMap.empty)
    mil' <- compiler a
    return mil'

-- execute local copy propagation
doLocalCopyPropagaion :: Mil -> LocalCopyPropagation Mil
doLocalCopyPropagaion mil@(Mil bbs) = do
    logMsgLn "Performing: Local Copy Propagation"
    bbs' <- mapM propagateCopies bbs
    return $ Mil bbs'
    where
        propagateCopies :: BasicBlock -> LocalCopyPropagation BasicBlock
        propagateCopies (BasicBlock bbid opcodes t) = do
            logMsgLn ("-- propagating copies in basic block " ++ show bbid)
            opcodes' <- propagateInOpCode opcodes
            return $ BasicBlock bbid opcodes' t
        propagateInOpCode :: [OpCode] -> LocalCopyPropagation [OpCode]
        propagateInOpCode (op:ops) = do
            logMsgLn ("-- propagating copies in opcode: " ++ show op)
            op' <- case op of
                Store sym val@(Const _) -> do
                    addCopy sym val
                    return op
                Store sym val -> do
                    val' <- propagateInValue val
                    addCopy sym val'
                    logMsgLn $ concat ["-- added ", sym, " as a copy of ", show val]
                    return $ Store sym val'
                Print val -> do
                    val' <- propagateInValue val
                    return $ Print val'
                _ -> return op
            logMsgLn ("-- opcode after copy propagation: " ++ show op')
            ops' <- propagateInOpCode ops
            return $ op':ops'
        propagateInOpCode [] = return []
        propagateInValue :: MilValue -> LocalCopyPropagation MilValue
        propagateInValue val = do
            logMsgLn ("-- propagating copies in: " ++ show val)
            case val of
                BinaryOp op lhs rhs -> do
                    lhs' <- propagateInValue lhs
                    rhs' <- propagateInValue rhs
                    return $ BinaryOp op lhs' rhs'
                Load sym -> do
                    v <- tableLookup sym
                    case v of
                        Just v' -> return v'
                        Nothing -> return val
                _ -> return val
        addCopy :: Symbol -> MilValue -> LocalCopyPropagation ()
        addCopy sym val = do
            s <- get
            let t = copyTable s
                t' = HashMap.insert sym val t
            put s{copyTable=t'}
        tableLookup :: Symbol -> LocalCopyPropagation (Maybe MilValue)
        tableLookup sym = do
            s <- get
            let t = copyTable s
                v = HashMap.lookup sym t
            return v
