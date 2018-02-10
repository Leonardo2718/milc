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

{- this module implements milc's IL generator -}

module MILGenerator where

import CompilerEnvironment
import MilcUtils
import MilcAST
import MIL

import Control.Monad.State


-- MIL generator state monad
data MilGenState = MilGenState { blockIdCounter :: BlockId }
type MilGenStateMonad = State MilGenState
type MilGenerator a = CompilerMonadT a MilGenStateMonad

-- initializer for MIL generator state
initMilGenState :: MilGenState
initMilGenState = MilGenState {blockIdCounter = 0}

-- returns a new, unique basic block ID
--
-- Returns the value of the basic block ID counter stored in the MIL generator
-- state and increments the stored value by one.
getBlockId :: MilGenerator BlockId
getBlockId = do
    s <- get
    let idCounter = blockIdCounter s
    put $ s{blockIdCounter=idCounter + 1}
    return idCounter

-- top level runner of MIL generator function
runMilGenerator :: Monad m => (AST -> MilGenerator a) -> AST -> CompilerMonadT (a, MilGenState) m
runMilGenerator ilgen ast = do
    let (c, s) = runState (runCompilerT . ilgen $ ast) (initMilGenState)
    a <- compiler c
    return (a, s)

-- generate MIL code from given AST
generateMil :: Monad m => AST -> CompilerMonadT (Mil, MilGenState) m
generateMil ast = do
    logMsgLn "=== Running MIL generator ==="
    milgen@(Mil mil,_) <- runMilGenerator genMil ast
    logMsgLn "MIL generation complete"
    logMil mil
    return milgen

-- helper for throwing MIL generation errors
milGenError :: String -> MilGenerator a
milGenError msg = logError ("MIL generation error: " ++ msg)

-- helper for creating a new basic block (with a unique ID)
generateBlock :: [OpCode] -> Terminator -> MilGenerator BasicBlock
generateBlock opcodes terminator = do
    bbId <- getBlockId
    return $ BasicBlock bbId opcodes terminator

-- helper for sets the terminator on a basic block
setBlockTerminator :: Terminator -> BasicBlock -> BasicBlock
setBlockTerminator terminator (BasicBlock bbId opcodes _) = BasicBlock bbId opcodes terminator

-- start MIL generation from the AST root node
genMil :: AST -> MilGenerator Mil
genMil ast@(AST _ stmt) = do
    logMsgLn "Walking AST: Generating MIL for top level statement"
    blocks <- genMilBasicBlocks Exit stmt
    return $ Mil blocks

-- generate a list of basic blocks from an AST statement node
--
-- All control flow internal to a statement should be contained within the basic
-- block list. Effectively, there should be no out-going control flow other than
-- jumps to the exit point and the fall-through of the last block in the list.
-- Control flow between blocks *within* a list is allowed.
--
-- Statements that map to a list of basic blocks are:
--  * IfThenElse
--  * WhileDo
--  * Block
--
-- All other statements map to single MIL opcodes (see genMilOpCode)
genMilBasicBlocks :: Terminator -> Statement -> MilGenerator [BasicBlock]
genMilBasicBlocks lastTerminator stmt = do
    logMsgLn "Walking statement"
    logTreeLines 6 stmt
    case stmt of
        IfThenElse expr thenStmt elseStmt _ -> do
            logMsgLn "-- generating merge point"
            mergePoint <- generateBlock [] lastTerminator
            logMil [mergePoint]
            logMsgLn "-- generating Else branch"
            elseBlocks <- genMilBasicBlocks Fallthrough elseStmt
            logMil elseBlocks
            logMsgLn "-- generating condition expression"
            condition <- genMilValue expr
            logMsgLn $ show condition
            case length elseBlocks == 1 && length (blockOpCodes . head $ elseBlocks) == 0 of
                True -> do
                    logMsgLn "-- Else branch is empty (only contains final terminator)"
                    logMsgLn "-- generating Then branch with Fallthrough to merge point"
                    thenBlocks <- genMilBasicBlocks Fallthrough thenStmt
                    logMil thenBlocks
                    case length thenBlocks of
                        1 -> do
                            logMsgLn "-- Then branch is also empty (only contains final terminator)"
                            logMsgLn "   NO NEED TO GENERATE ANYTHING"
                            return []
                        _ -> do
                            logMsgLn "-- generating condition check with Branch to merge point"
                            checkCondition <- generateBlock [] (BranchZero condition (blockId mergePoint))
                            logMil [checkCondition]
                            return $ checkCondition:thenBlocks ++ [mergePoint]
                _ -> do
                    logMsgLn "-- generating Then branch with Jump to merge point"
                    thenBlocks <- genMilBasicBlocks (Jump (blockId mergePoint)) thenStmt
                    logMil thenBlocks
                    logMsgLn "-- generating condition check with Branch to Else"
                    checkCondition <- generateBlock [] (BranchZero condition (blockId $ head elseBlocks))
                    logMil [checkCondition]
                    return $ checkCondition:thenBlocks ++ elseBlocks ++ [mergePoint]
        WhileDo expr stmt _ -> do
            logMsgLn "-- Generating loop exit block"
            exitBlock <- generateBlock [] lastTerminator
            logMsgLn "-- Generating loop condtion"
            condition <- genMilValue expr
            logMsgLn "-- Generating loop condition check"
            checkCondition <- generateBlock [] (BranchZero condition (blockId exitBlock))
            loopBody <- genMilBasicBlocks (Jump (blockId checkCondition)) stmt
            if length loopBody == 1 then do
                logMsgLn "-- Loop body is empty"
                logMsgLn "   NO NEED TO GENERATE ANYTHING"
                return []
            else do
                return $ checkCondition:loopBody ++ [exitBlock]
        Block stmts _ -> do
            logMsgLn "-- will walk all sub-Statements"
            blocks <- mapM (genMilBasicBlocks Fallthrough) $ stmts
            logMsgLn "-- generated BasicBlocks for Block statement"
            mapM logMil blocks
            logMsgLn "-- generating final Block statement terminator"
            terminatorBlock <- generateBlock [] lastTerminator
            logMil [terminatorBlock]
            logMsgLn "-- merging single BasicBlock statements"
            let mergedBlocks = mergeSingleBlocks blocks terminatorBlock
            mapM logMil mergedBlocks
            return . concat $ mergedBlocks
            where
                {-
                Because statements that map to single MIL opcodes end up in a
                list containing just one basic block, containing just one
                opcode, there is a tendency to generate lots small basic block.
                There is therefore some benefit in merging some of these blocks
                together to reduce their numbers.

                Given how the basic block lists are generated by this function,
                two basic blocks in two lists can be safely *if and only if* the
                two lists only contain one basic block each. Because there
                is only one block in each list, there cannot be any control flow
                between blocks within each list. There is therefore no risk of
                breaking control flow by merging the two basic blocks as long as
                merging is done correctly (as defined  for `mergeBasicBlocks`).
                -}
                mergeSingleBlocks :: [[BasicBlock]] -> BasicBlock -> [[BasicBlock]]
                mergeSingleBlocks ([b1]:[b2]:bss) t = mergeSingleBlocks ([mergeBasicBlocks b1 b2]:bss) t
                mergeSingleBlocks [] t = [[t]]
                mergeSingleBlocks (bs:bss) t = bs:mergeSingleBlocks bss t
        _ -> do
            -- If the statement doesn't match any of the above, then it must be
            -- one of those that maps to a single MIL opcode. So, we generate
            -- the opcode, wrap it up in a new basic block, and return a list
            -- containing only that single basic block.
            opcode <- genMilOpCode stmt
            block <- generateBlock [opcode] lastTerminator
            return [block]

-- generate a MIL opcode from an AST statement
--
-- Statements that map to MIL opcodes are:
--  * Input -> InputOp
--  * Write -> WriteOp
--  * Assign -> Store
genMilOpCode :: Statement -> MilGenerator OpCode
genMilOpCode stmt = do
    logMsgLn "Generating opcode from simple Statement"
    opcode <- case stmt of
        Input name _ -> do
            logMsgLn "-- found Input statement"
            return (Read name)
        Write expr _ -> do
            logMsgLn "-- found Write statement"
            val <- genMilValue expr
            return (Print val)
        Assign name expr _ -> do
            logMsgLn "-- found Assign statement"
            val <- genMilValue expr
            return (Store name val)
        _ -> do
            milGenError $ "Unexpected statement\n" ++ show stmt
    logMsgLn ("-- generated OpCode: " ++ show opcode)
    return opcode

-- generate a MIL value
--
-- All AST expression nodes exclusively map to MIL values. The structure of MIL
-- values reflects the recursive nature of AST nodes. MIL value generation is
-- there fore also done recursively.
genMilValue :: Expression -> MilGenerator MilValue
genMilValue expr = do
    logMsgLn "   generating MilValue from Expression"
    logTree expr
    val <- case expr of
        Id name _ -> return $ Load name
        Num val _ -> return $ Const val
        Add le re _ -> do
            lval <- genMilValue le
            rval <- genMilValue re
            return $ BinaryOp AddOp lval rval
        Sub le re _ -> do
            lval <- genMilValue le
            rval <- genMilValue re
            return $ BinaryOp SubOp lval rval
        Mul le re _ -> do
            lval <- genMilValue le
            rval <- genMilValue re
            return $ BinaryOp MulOp lval rval
        Div le re _ -> do
            lval <- genMilValue le
            rval <- genMilValue re
            return $ BinaryOp DivOp lval rval
    logMsgLn $ "   generated MilValue: " ++ show val
    return val
