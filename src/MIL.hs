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

{- this module define mcomp's Intermediate Language -}

module MIL where

import CompilerEnvironment
import MRDParser

import Data.List
import Control.Monad.State

type Symbol = String
data BinaryOp = AddOp | SubOp | MulOp | DivOp deriving (Eq, Show)
data MilValue   = BinaryOp BinaryOp MilValue MilValue
                | Const Int
                | Load Symbol deriving (Eq, Show)

data OpCode = InputOp Symbol
            | WriteOp MilValue
            | Store Symbol MilValue
            | Call Symbol [MilValue] deriving (Eq, Show)

type BlockId = Int
data Terminator = Jump {jumpTarget :: BlockId}
                | Branch {jumpCondition :: MilValue, jumpTarget :: BlockId}
                | BranchZero {jumpCondition :: MilValue, jumpTarget :: BlockId}
                | Fallthrough
                | Exit deriving (Eq, Show)

data BasicBlock = BasicBlock    { blockId :: BlockId
                                , blockOpCodes :: [OpCode]
                                , blockTerm :: Terminator
                                }

instance Show BasicBlock where
    show bb = intercalate "\n" . concat $ [ [bbHeader]
                                 , map showWithPadding (blockOpCodes bb)
                                 , [termLine]
                                 , [term]
                                 ] where
        bbHeader = "BasicBlock " ++ show (blockId bb)
        termLine = padding ++ take (length term - length padding) (repeat '~')
        term = showWithPadding (blockTerm bb)
        showWithPadding :: Show a => a -> String
        showWithPadding = (\ s -> padding ++ s) . show
        padding = "    "

data Mil = Mil [BasicBlock]

instance Show Mil where
    show (Mil bbs) = showMil bbs

showMil :: [BasicBlock] -> String
showMil bbs = intercalate "\n" . map show $ bbs

data MilGenState = MilGenState { blockIdCounter :: BlockId }
type MilGenStateMonad = State MilGenState
type MilGenerator a = CompilerMonadT a MilGenStateMonad

initMilGenState :: MilGenState
initMilGenState = MilGenState {blockIdCounter = 0}

getBlockIdNoIncrement:: MilGenerator BlockId
getBlockIdNoIncrement = do
    s <- get
    return $ blockIdCounter s

getBlockId :: MilGenerator BlockId
getBlockId = do
    s <- get
    let idCounter = blockIdCounter s
    put $ s{blockIdCounter=idCounter + 1}
    return idCounter

runMilGenerator :: (AST -> MilGenerator a) -> AST -> CompilerMonad (a, MilGenState)
runMilGenerator ilgen ast = do
    let (c, s) = runState (runCompilerT . ilgen $ ast) (initMilGenState)
    a <- compiler c
    return (a, s)

generateMil :: AST -> CompilerMonad (Mil, MilGenState)
generateMil ast = do
    logMsgLn "=== Running MIL generator ==="
    milgen@(Mil mil,_) <- runMilGenerator genMil ast
    logMsgLn "MIL generation complete"
    logMil mil
    return milgen

logMil :: Monad m => [BasicBlock] -> CompilerMonadT () m
logMil bbs = do
    logMsgLn "--------------------------------------------------"
    logMsgLn $ showMil bbs
    logMsgLn "--------------------------------------------------"

logMilLines :: Monad m => Int -> [BasicBlock] -> CompilerMonadT () m
logMilLines l bbs = do
    logMsgLn "--------------------------------------------------"
    logMsgLn $ (asLines . lines . showMil $ bbs)
    logMsgLn "--------------------------------------------------"
    where
        asLines ls = if length ls > l
            then (unlines . take l $ ls) ++ "  ..."
            else intercalate "\n" ls

milGenError :: String -> MilGenerator a
milGenError msg = logError ("MIL generation error: " ++ msg)

generateBlock :: [OpCode] -> Terminator -> MilGenerator BasicBlock
generateBlock opcodes terminator = do
    bbId <- getBlockId
    return $ BasicBlock bbId opcodes terminator

setBlockTerminator :: Terminator -> BasicBlock -> BasicBlock
setBlockTerminator terminator (BasicBlock bbId opcodes _) = BasicBlock bbId opcodes terminator

mergeBasicBlocks :: BasicBlock -> BasicBlock -> BasicBlock
mergeBasicBlocks (BasicBlock id1 opcodes1 _) (BasicBlock _ opcodes2 terminator) = BasicBlock id1 (opcodes1 ++ opcodes2) terminator

genMil :: AST -> MilGenerator Mil
genMil ast@(AST _ stmt) = do
    logMsgLn "Walking AST: Generating MIL for top level statement"
    blocks <- genMilBasicBlocks Exit stmt
    return $ Mil blocks

genMilBasicBlocks :: Terminator -> Statement -> MilGenerator [BasicBlock]
genMilBasicBlocks lastTerminator stmt = do
    logMsgLn "Walking statement"
    logTreeLines 6 stmt
    case stmt of
        IfThenElse expr thenStmt elseStmt _ -> do
            -- logMsgLn "Walking IfThenElse statement"
            logMsgLn "-- generating merge point"
            mergePoint <- generateBlock [] lastTerminator
            logMil [mergePoint]
            logMsgLn "-- generating Else branch"
            elseBlocks <- genMilBasicBlocks Fallthrough elseStmt
            logMil elseBlocks
            logMsgLn "-- generating condition expression"
            condition <- genMilValue expr
            logMsgLn $ show condition
            case length elseBlocks of
                1 -> do
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
            -- logMsgLn "Walking WhileDo statement"
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
            -- logMsgLn "Walking Block statement"
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
                mergeSingleBlocks :: [[BasicBlock]] -> BasicBlock -> [[BasicBlock]]
                mergeSingleBlocks ([b1]:[b2]:bss) t = mergeSingleBlocks ([mergeBasicBlocks b1 b2]:bss) t
                mergeSingleBlocks [] t = [[t]]
                mergeSingleBlocks (bs:bss) t = bs:mergeSingleBlocks bss t
        _ -> do
            opcode <- genMilOpCode stmt
            block <- generateBlock [opcode] lastTerminator
            return [block]

genMilOpCode :: Statement -> MilGenerator OpCode
genMilOpCode stmt = do
    logMsgLn "Generating opcode from simple Statement"
    opcode <- case stmt of
        Input name _ -> do
            logMsgLn "-- found Input statement"
            return (InputOp name)
        Write expr _ -> do
            logMsgLn "-- found Write statement"
            val <- genMilValue expr
            return (WriteOp val)
        Assign name expr _ -> do
            logMsgLn "-- found Assign statement"
            val <- genMilValue expr
            return (Store name val)
        _ -> do
            milGenError $ "Unexpected statement\n" ++ show stmt
    logMsgLn ("-- generated OpCode: " ++ show opcode)
    return opcode

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
