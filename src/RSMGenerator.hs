
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
import MIL

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
    show (RSMCode opcodes) = unlines . map showWithPadding $ opcodes where
        showWithPadding l@(LABEL _) = show l
        showWithPadding op = "    " ++ show op

generateRSMCode :: Mil -> CompilerMonad RSMCode
generateRSMCode (Mil bbs) = return . RSMCode . concat . map fromBasicBlock $ bbs

fromBasicBlock :: BasicBlock -> [RSMOpCode]
fromBasicBlock (BasicBlock bid opcodes terminator) = LABEL (Label bid) : (concat $ map fromOpCode opcodes) ++ fromTerminator terminator

fromTerminator :: Terminator -> [RSMOpCode]
fromTerminator t = case t of
    Jump target -> [JUMP (Label target)]
    BranchZero val target -> fromMilValue val ++ [CJUMP (Label target)]
    _ -> []

fromOpCode :: OpCode -> [RSMOpCode]
fromOpCode opcode = case opcode of
    InputOp name -> [READ name]
    WriteOp val -> fromMilValue val
    Store name val -> fromMilValue val ++ [LOAD name]

fromMilValue :: MilValue -> [RSMOpCode]
fromMilValue val = case val of
    BinaryOp op lhs rhs -> fromMilValue lhs ++ fromMilValue rhs ++ [OP 2 (fromBinaryOp op)]
    Const v -> [CPUSH v]
    Load n -> [RPUSH n]

fromBinaryOp :: BinaryOp -> String
fromBinaryOp AddOp = "+"
fromBinaryOp SubOp = "-"
fromBinaryOp MulOp = "*"
fromBinaryOp DivOp = "/"
