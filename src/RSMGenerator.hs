
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

{- This module implements a code generator for Robin's Stack Machine in csh -}

module RSMGenerator where

import CompilerEnvironment
import MilcUtils
import MIL

import Data.List

type Register = String
data Label = Label Int deriving (Eq)
data RSMOpCode  = CPUSH Int
                | RPUSH Register
                | SPUSH
                | LOAD Register
                | OP Int String
                | CJUMP Label
                | JUMP Label
                | PRINT
                | READ Register
                | LABEL Label
                deriving (Eq)
data RSMCode = RSMCode [RSMOpCode] deriving (Eq)

instance Show Label where
    show (Label l) = "L" ++ show l

instance Show RSMOpCode where
    show opcode = case opcode of
        CPUSH val -> "cPUSH " ++ show val
        RPUSH name -> "rPUSH " ++ name
        SPUSH -> "sPUSH"
        LOAD name -> "LOAD " ++ name
        OP i op -> "OP" ++ show i ++ " " ++ op
        CJUMP l -> "cJUMP " ++ show l
        JUMP l -> "JUMP " ++ show l
        PRINT -> "PRINT"
        READ name -> "READ " ++ name
        LABEL l -> show l ++ ":"

instance Show RSMCode where
    show (RSMCode opcodes) = intercalate "\n" . map showWithPadding $ opcodes where
        showWithPadding l@(LABEL _) = show l
        showWithPadding op = "    " ++ show op

logRSMCode :: Monad m => [RSMOpCode] -> CompilerMonadT () m
logRSMCode = logBlock . show . RSMCode -- do

generateRSMCode :: Mil -> CompilerMonad RSMCode
generateRSMCode (Mil bbs) = do
    logMsgLn "=== Running code generation for RSM ==="
    logMsgLn "Generating code from MIL"
    codes <- mapM fromBasicBlock bbs
    logMsgLn "Code generation successful"
    logRSMCode . concat $ codes
    return . RSMCode . concat $ codes

fromBasicBlock :: BasicBlock -> CompilerMonad [RSMOpCode]
fromBasicBlock bb@(BasicBlock bid opcodes terminator) = do
    logMsgLn "Generating code for BasicBlock"
    logMil [bb]
    codes <- mapM fromOpCode opcodes
    term <- fromTerminator terminator
    let codes' = LABEL (Label bid) : (concat codes) ++ term
    logRSMCode codes'
    return codes'

fromTerminator :: Terminator -> CompilerMonad [RSMOpCode]
fromTerminator t = do
    logMsgLn $ "Generating code for Terminator: "  ++ show t
    codes <- case t of
        Jump target -> do
            let code = JUMP (Label target)
            return [code]
        BranchZero val target -> do
            codes <- fromMilValue val
            return $ codes ++ [CJUMP (Label target)]
        Fallthrough -> do
            logMsgLn "-- No need to generate anything for Fallthrough"
            return []
        Exit -> do
            logMsgLn "-- No need to generate anything for Exit"
            return []
        _ -> logError $ "Unrecognized terminator: " ++ show t
    logRSMCode codes
    return codes

fromOpCode :: OpCode -> CompilerMonad [RSMOpCode]
fromOpCode opcode = do
    logMsgLn $ "Generating code for OpCode: " ++ show opcode
    codes <- case opcode of
        InputOp name -> return [READ name]
        WriteOp val -> do
            codes <- fromMilValue val
            return $ codes ++ [PRINT]
        Store name val -> do
            codes <- fromMilValue val
            return $ codes ++ [LOAD name]
    logRSMCode codes
    return codes

fromMilValue :: MilValue -> CompilerMonad [RSMOpCode]
fromMilValue val = do
    logMsgLn $ "Generating code from MilValue: " ++ show val
    codes <- case val of
        BinaryOp op lhs rhs -> do
            lhsCodes <- fromMilValue lhs
            rhsCodes <- fromMilValue rhs
            return $ lhsCodes ++ rhsCodes ++ [OP 2 (fromBinaryOp op)]
        Const v -> return [CPUSH v]
        Load n -> return [RPUSH n]
    logRSMCode codes
    return codes

fromBinaryOp :: BinaryOp -> String
fromBinaryOp AddOp = "+"
fromBinaryOp SubOp = "-"
fromBinaryOp MulOp = "*"
fromBinaryOp DivOp = "/"
