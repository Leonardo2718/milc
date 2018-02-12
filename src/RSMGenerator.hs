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

{- This module implements a code generator for Robin's Stack Machine (RSM) in csh -}

module RSMGenerator where

import CompilerEnvironment
import MilcUtils
import MIL
import MEncoder
import MilcCFG

import Data.List
import System.IO


-- data type for srepresenting RSM code
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

-- RSM data type instantiations
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

-- make RSMCode an instance of TargetCode so we can write it to a file easily
instance TargetCode RSMCode where
    encodeToFile code h = hPutStrLn h (cshHeader ++ show code) where
        -- this adds the csh implementation of RSM to the top of file so that
        -- executing the generated code is as easy as sourceing the file
        cshHeader = "#! /bin/csh \n\
                    \\n\
                    \# ... aliases for Robin's stack machine in csh. \n\
                    \# This file should be \"sourced\" prior to executing \n\
                    \# stack machine files. \n\
                    \ \n\
                    \set stack = \"\" \n\
                    \alias cPUSH           'set stack = (\\!:1 $stack)' \n\
                    \alias rPUSH           'set stack = ($\\!:1 $stack)' \n\
                    \alias sPUSH           '@ stack[1] = $stack[1] + 1 ; set stack[1] = $stack[$stack[1]]' \n\
                    \alias LOAD            'eval \"set \\!:1 = \\$stack[1] ; shift stack\"' \n\
                    \alias OP2             'eval \"@ stack[2] = \\$stack[2] \\!:1 \\$stack[1]\"; shift stack' \n\
                    \alias cJUMP           'set tos = $stack[1]; shift stack; if ($tos == 0) goto \\!:1' \n\
                    \alias JUMP             goto \n\
                    \alias PRINT           'echo $stack[1]; shift stack' \n\
                    \alias READ            'eval \"set \\!:1 = $< \" ' \n\n"

-- helper for logging RSM code as a block
logRSMCode :: Monad m => [RSMOpCode] -> CompilerMonadT () m
logRSMCode = logBlock . show . RSMCode -- do

-- start code generation from top level MIL code
generateRSMCode :: Monad m => Mil -> CompilerMonadT RSMCode m
generateRSMCode mil@(Mil bbs) = do
    logMsgLn "=== Running code generation for RSM ==="
    logMsgLn "Building CFG"
    cfg <- buildCFG mil
    logCFG cfg
    logMsgLn "Generating code from MIL"
    codes <- mapM (fromBasicBlock cfg) bbs
    logMsgLn "Code generation successful"
    logRSMCode . concat $ codes
    return . RSMCode . concat $ codes

-- generate code from a basic block
fromBasicBlock :: Monad m => CFG -> BasicBlock -> CompilerMonadT [RSMOpCode] m
fromBasicBlock cfg bb@(BasicBlock bid opcodes terminator) = do
    logMsgLn "Generating code for BasicBlock"
    logMil [bb]
    ins <- incomingEdgesOf bb cfg
    logMsgLn ("-- incoming edges are: " ++ show ins)
    let isBranchJumpTarget = case ins of
            [IncomingFallthrough _] -> False
            [Start] -> False
            _ -> True
    case isBranchJumpTarget of
        True -> logMsgLn "   block is target of branch/jump: will generated label"
        False -> logMsgLn "   block is not target of branch/jump: won't generated label"
    codes <- mapM fromOpCode opcodes
    term <- fromTerminator terminator
    let codes' = if isBranchJumpTarget
            -- only need to generate a label if block is target of a branch/jump
            then LABEL (Label bid) : (concat codes) ++ term
            else (concat codes) ++ term
    logRSMCode codes'
    return codes'

-- generate code from a terminator
fromTerminator :: Monad m => Terminator -> CompilerMonadT [RSMOpCode] m
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
            logMsgLn "-- no need to generate anything for Fallthrough"
            return []
        Return _ -> do
            logMsgLn "-- no need to generate anything for Return"
            return []
        _ -> logError $ "Unrecognized terminator: " ++ show t
    logRSMCode codes
    return codes

-- generate code from a MIL opcode
fromOpCode :: Monad m =>  OpCode -> CompilerMonadT [RSMOpCode] m
fromOpCode opcode = do
    logMsgLn $ "Generating code for OpCode: " ++ show opcode
    codes <- case opcode of
        Read name -> return [READ name]
        Print val -> do
            codes <- fromMilValue val
            return $ codes ++ [PRINT]
        Store name val -> do
            codes <- fromMilValue val
            return $ codes ++ [LOAD name]
    logRSMCode codes
    return codes

-- generate code from a MIL value
fromMilValue :: Monad m =>  MilValue -> CompilerMonadT [RSMOpCode] m
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

-- return RSM operator for a MIL binary operator
fromBinaryOp :: BinaryOp -> String
fromBinaryOp AddOp = "+"
fromBinaryOp SubOp = "-"
fromBinaryOp MulOp = "*"
fromBinaryOp DivOp = "/"
