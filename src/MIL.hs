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

{- this module define milc's Intermediate Language: MIL -}

module MIL where

import CompilerEnvironment
import MilcUtils

import Data.List


-- MIL symbol data type
type Symbol = String

-- MIL binary operations
data BinaryOp = AddOp | SubOp | MulOp | DivOp deriving (Eq, Show)

-- MIL value representation
data MilValue   = BinaryOp BinaryOp MilValue MilValue   -- result of binary operation
                | Const Int                             -- constant value
                | Load Symbol                           -- result of loading a symbol
                deriving (Eq, Show)

-- MIL opcodes
data OpCode = Read Symbol
            | Print MilValue
            | Store Symbol MilValue
            | Call Symbol [MilValue]
            deriving (Eq, Show)

-- MIL basic block ID
type BlockId = Int

-- MIL basic block terminators
data Terminator = Jump {jumpTarget :: BlockId}
                | Branch {jumpCondition :: MilValue, jumpTarget :: BlockId}
                | BranchZero {jumpCondition :: MilValue, jumpTarget :: BlockId}
                | Fallthrough
                | Return {returnValue :: Maybe MilValue}
                deriving (Eq, Show)

-- MIL basic block representation
data BasicBlock = BasicBlock    { blockId :: BlockId
                                , blockOpCodes :: [OpCode]
                                , blockTerm :: Terminator
                                }

-- top level MIL data type
data Mil = Mil [BasicBlock]

-- MIL data type instantiations
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

instance Show Mil where
    show (Mil bbs) = showMil bbs

-- show string representation of MIL code
showMil :: [BasicBlock] -> String
showMil bbs = intercalate "\n" . map show $ bbs

-- helper for logging blocks of MIL
logMil :: Monad m => [BasicBlock] -> CompilerMonadT () m
logMil = logBlock . showMil

-- helper for logging the first n lines of blocks of MIL
logMilLines :: Monad m => Int -> [BasicBlock] -> CompilerMonadT () m
logMilLines n = logBlockLines n . showMil

-- helper for merging to basic blocks correctly
--
-- Given two basic blocks:
--
--      +------------------------+
--      | BasicBlock_1           |
--      | opcodes_1              |
--      | terminator_1           |
--      +------------------------+
--                  |
--      +------------------------+
--      | BasicBlock_2           |
--      | opcodes_2              |
--      | terminator_2           |
--      +------------------------+
--
-- the correct way of merging them is to create a new block with:
--  * the same ID as the first block
--  * the opcodes of the first block concatenated with the opcodes of the second
--  * the same terminator as the second block
--
--      +------------------------+
--      | BasicBlock_1           |
--      | opcodes_1 ++ opcodes_2 |
--      | terminator_2           |
--      +------------------------+
--
mergeBasicBlocks :: BasicBlock -> BasicBlock -> BasicBlock
mergeBasicBlocks (BasicBlock id1 opcodes1 _) (BasicBlock _ opcodes2 terminator) = BasicBlock id1 (opcodes1 ++ opcodes2) terminator
