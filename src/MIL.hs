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
-- import Data.HashMap.Strict

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
                | BranchNot {jumpCondition :: MilValue, jumpTarget :: BlockId}
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
        termLine = padding ++ take (length term - length padding) (repeat '-')
        term = showWithPadding (blockTerm bb)
        showWithPadding :: Show a => a -> String
        showWithPadding = (\ s -> padding ++ s) . show
        padding = "    "

data Mil = Mil [BasicBlock]

instance Show Mil where
    show (Mil bbs) = intercalate "\n" . map show $ bbs

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
    mil <- runMilGenerator genMil ast
    logMsgLn "MIL generation complete"
    return mil

milGenError :: String -> MilGenerator a
milGenError msg = logError ("MIL generation error: " ++ msg)

generateBlock :: [OpCode] -> Terminator -> MilGenerator BasicBlock
generateBlock opcodes terminator = do
    bbId <- getBlockId
    return $ BasicBlock bbId opcodes terminator

setBlockTerminator :: Terminator -> BasicBlock -> BasicBlock
setBlockTerminator terminator (BasicBlock bbId opcodes _) = BasicBlock bbId opcodes terminator

genMil :: AST -> MilGenerator Mil
genMil (AST _ stmt) = do
    logMsgLn "Walking AST\nGenerating MIL for top level statement"
    blocks <- genMilBasicBlocks Exit stmt
    return $ Mil blocks

genMilBasicBlocks :: Terminator -> Statement -> MilGenerator [BasicBlock]
genMilBasicBlocks lastTerminator stmt = do
    case stmt of
        IfThenElse expr thenStmt elseStmt _ -> do
            logMsgLn "Walking IfThenElse statement"
            logMsgLn "-- generating merge point"
            mergePoint <- generateBlock [] lastTerminator
            thenBlocks <- genMilBasicBlocks (Jump (blockId mergePoint)) thenStmt
            condition <- genMilExpression expr
            elseBlocks <- genMilBasicBlocks Fallthrough elseStmt
            checkCondition <- generateBlock [] (BranchNot condition (blockId (head elseBlocks)))
            return $ checkCondition:thenBlocks ++ elseBlocks ++ [mergePoint]
        WhileDo expr stmt _ -> do
            logMsgLn "Walking WhileDo statement"
            logMsgLn "-- Generating loop exit block"
            exitBlock <- generateBlock [] lastTerminator
            logMsgLn "-- Generating loop condtion"
            condition <- genMilExpression expr
            logMsgLn "-- Generating loop condition check"
            checkCondition <- generateBlock [] (BranchNot condition (blockId exitBlock))
            loopBody <- genMilBasicBlocks (Jump (blockId checkCondition)) stmt
            if length loopBody == 0 then do
                logMsgLn "-- Loop body is empty so won't generate MIL for it"
                return []
            else do
                return $ checkCondition:loopBody ++ [exitBlock]
        Block stmts _ -> do
            logMsgLn "Walking Block statement"
            blocks <- mapM (genMilBasicBlocks Fallthrough) $ stmts
            terminatorBlock <- generateBlock [] lastTerminator
            return $ concat blocks ++ [terminatorBlock]
        _ -> do
            opcode <- genMilOpCode stmt
            block <- generateBlock [opcode] lastTerminator
            return [block]

genMilOpCode :: Statement -> MilGenerator OpCode
genMilOpCode stmt = do
    logMsgLn "Attempting to generate opcode from a simple statement"
    opcode <- case stmt of
        Input name _ -> do
            logMsgLn "-- Found Input statement"
            return (InputOp name)
        Write expr _ -> do
            logMsgLn "-- Found Write statement"
            val <- genMilExpression expr
            return (WriteOp val)
        Assign name expr _ -> do
            logMsgLn "-- Found Assign statement"
            val <- genMilExpression expr
            return (Store name val)
        _ -> do
            logMsgLn "-- Found unexpected (non-simple) statement"
            logTree stmt
            milGenError "Found unexpected (non-simple) statement"
    logMsgLn ("-- successfully generated opcode: " ++ show opcode)
    return opcode

genMilExpression :: Expression -> MilGenerator MilValue
genMilExpression (Id name _) = return $ Load name
genMilExpression (Num val _) = return $ Const val
genMilExpression (Add le re _) = do
    lval <- genMilExpression le
    rval <- genMilExpression re
    return $ BinaryOp AddOp lval rval
genMilExpression (Sub le re _) = do
    lval <- genMilExpression le
    rval <- genMilExpression re
    return $ BinaryOp SubOp lval rval
genMilExpression (Mul le re _) = do
    lval <- genMilExpression le
    rval <- genMilExpression re
    return $ BinaryOp MulOp lval rval
genMilExpression (Div le re _) = do
    lval <- genMilExpression le
    rval <- genMilExpression re
    return $ BinaryOp DivOp lval rval