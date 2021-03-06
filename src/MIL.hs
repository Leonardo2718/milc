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


-- MIL data types
data MilType = I32 | F32 | Char | Bool | Pointer | StackPointer | HeapPointer deriving (Eq, Show)

-- MIL symbol data type
data Symbol = StackLocal {symbolName :: String, symbolType :: MilType, frameOffset :: Int, staticLink :: Int}
            | FunctionLabel {symbolName :: String, returnType :: Maybe MilType, parameterTypes :: [MilType], staticLink :: Int}
            deriving (Eq, Show)

-- MIL binary operations
data BinaryOp = AddOp | SubOp | MulOp | DivOp | EQOp | LTOp | LEOp | GTOp | GEOp | AndOp | OrOp deriving (Eq, Show)

-- MIL unary operations
data UnaryOp = NegativeOp | BooleanNotOp | FloatOp | FloorOp | CeilingOp deriving (Eq, Show)

-- MIL value representation
data MilValue   = BinaryOp MilType BinaryOp MilValue MilValue   -- result of binary operation
                | UnaryOp MilType UnaryOp MilValue              -- result of unary operation
                | ConstI32 Int                                  -- constant 32-bit integer value
                | ConstF32 Float                                -- constant 32-bit floating point value (IEEE-754)
                | ConstChar Char                                -- constant character value
                | ConstBool Bool                                -- constant boolean value
                | Load Symbol                                   -- result of loading a variable
                | LoadOffset                                    -- result of loading a value at an offset
                    { valueType :: MilType
                    , valueName :: Symbol
                    , valueOffset :: MilValue
                    , valueStaticLink :: MilValue
                    }
                -- | StackLoad MilValue                            -- result of loading a value from the stack
                -- | HeapLoad MilValue                             -- result of loading a value from the heap
                | Call Symbol [MilValue]                        -- result of calling a function
                deriving (Eq, Show)

milTypeOf :: MilValue -> MilType
milTypeOf op = case op of
    BinaryOp t _ _ _ -> t
    UnaryOp t _ _ -> t
    ConstI32 _ -> I32
    ConstF32 _ -> F32
    ConstChar _ -> Char
    ConstBool _ -> Bool
    Load (StackLocal _ t _ _) -> t
    LoadOffset t _ _ _ -> t
    Call (FunctionLabel _ (Just t) _ _) _ -> t

showMilValue :: String -> MilValue -> String
showMilValue lead val = lead ++ case val of
    BinaryOp tt op lhs rhs -> concat
        [ "BinaryOp ", show tt, " ", show op, "\n"
        , showMilValue (lead ++ "  ") lhs, "\n"
        , showMilValue (lead ++ "  ") rhs
        ]
    UnaryOp tt op subVal -> concat
        [ "UnaryOp ", show tt, " ", show op, "\n"
        , showMilValue (lead ++ "  ") subVal
        ]
    Call label args -> concat ([ "Call ", show label, if null args then "" else "\n"] ++ intersperse "\n" (map (showMilValue (lead ++ "  ")) args))
    _ -> show val


-- MIL opcodes
data OpCode = Read Symbol
            | Print MilType MilValue
            | Store Symbol MilValue
            | StoreOffset
                { destType :: MilType
                , destName :: Symbol
                , destOffset :: MilValue
                , destStaticLink :: MilValue
                , targetVal :: MilValue
                }
            -- | Call (Maybe Symbol) Symbol [MilValue]
            | AllocateSlots {localTypes :: [MilType]}
            | ReleaseSlots {localTypes :: [MilType]}
            | PushBlock
            | PopBlock
            deriving (Eq, Show)

showOpCode :: String -> String -> OpCode -> String
showOpCode lead valLead opcode = lead ++ case opcode of
    Print tt val -> concat
        [ "Print ", show tt, "\n"
        , showMilValue (lead ++ valLead) val
        ]
    Store sym val -> concat
        [ "Store ", show sym, "\n"
        , showMilValue (lead ++ valLead) val
        ]
    _ -> show opcode

-- MIL basic block ID
type BlockId = Int

-- MIL basic block terminators
data Terminator = Jump {jumpTarget :: BlockId}
                | Branch {jumpCondition :: MilValue, jumpTarget :: BlockId}
                | BranchZero {jumpCondition :: MilValue, jumpTarget :: BlockId}
                | Fallthrough
                | Return {returnValue :: Maybe (MilType, MilValue)}
                | Exit {exitValue :: Maybe MilValue}
                deriving (Eq, Show)

showTerminator :: String -> String -> Terminator -> String
showTerminator lead valLead terminator = concat
    [ lead, termLine, "\n"
    , lead, termHeader, "\n"
    , vals, if null vals then "" else "\n"
    ]
    where
        termLine = take (length termHeader) (repeat '~')
        (termHeader, vals) = case terminator of
            Branch condition bid -> ("Branch " ++ show bid, showMilValue (lead ++ valLead) condition)
            BranchZero condition bid -> ("BranchZero " ++ show bid, showMilValue (lead ++ valLead) condition)
            Return (Just (tt, val)) -> ("Return " ++ show tt, showMilValue (lead ++ valLead) val)
            Return Nothing -> ("Return Nothing", "")
            _ -> (show terminator, "")

-- MIL basic block representation
data BasicBlock = BasicBlock    { blockId :: BlockId
                                , blockOpCodes :: [OpCode]
                                , blockTerm :: Terminator
                                }

-- MIL function representation
data Function = Function    { functionLabel :: String
                            , functionRetType :: Maybe MilType
                            , functionParamTypes :: [MilType]
                            , functionLocalTypes :: [MilType]
                            , functionBody :: [BasicBlock]
                            }

-- top level MIL data type
data Mil = Mil [Function]

-- MIL data type instantiations
instance Show BasicBlock where
    show bb = intercalate "\n" . concat $ [ [bbHeader]
                                 , map (showOpCode padding valPadding) (blockOpCodes bb)
                                 , [term]
                                 ] where
        bbHeader = "BasicBlock " ++ show (blockId bb)
        term = showTerminator padding valPadding (blockTerm bb)
        showWithPadding :: Show a => a -> String
        showWithPadding = (\ s -> padding ++ s) . show
        padding = "    "
        valPadding = padding ++ "`-|"

instance Show Function where
    show (Function label rt paramt localt body) = concat
        [ label, " ", show rt, " ", show paramt, "\n"
        , "(", show localt, ")\n"
        , concat (take (length label) (repeat "=")), "\n"
        , showBBs body
        ]

instance Show Mil where
    show (Mil funs) = intercalate "\n" (map show funs)

-- show string representation of MIL code
showBBs :: [BasicBlock] -> String
showBBs bbs = intercalate "\n" . map show $ bbs

-- helper for logging blocks of MIL
logBBs :: Monad m => [BasicBlock] -> CompilerMonadT () m
logBBs = logBlock . showBBs

-- helper for logging the first n lines of blocks of MIL
logBBLines :: Monad m => Int -> [BasicBlock] -> CompilerMonadT () m
logBBLines n = logBlockLines n . showBBs

logMil :: Monad m => Mil -> CompilerMonadT () m
logMil = logBlock . show

logFunction :: Monad m => Function -> CompilerMonadT () m
logFunction = logBlock . show

-- helper for merging to basic blocks correctly
--
-- Given two basic blocks:
--
--      +--------------+     +--------------+
--      | BasicBlock_1 |     | BasicBlock_2 |
--      | opcodes_1    |---->| opcodes_2    |
--      | terminator_1 |     | terminator_2 |
--      +--------------+     +--------------+
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
