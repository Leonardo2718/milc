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


{- This module define the code generator for AM -}

module AMCodeGenerator where

import CompilerEnvironment
import MilcUtils
import MIL
import MEncoder

import Control.Monad.Identity
import System.IO

type AMLabel = String

data AMRegister = AMStackPointer | AMFramePointer | AMCodePointer deriving (Eq)

instance Show AMRegister where
    show AMStackPointer = "%sp"
    show AMFramePointer = "%fp"
    show AMCodePointer = "%cp"

data AMOperation    = ADD_F | SUB_F | DIV_F | MUL_F | NEG_F | FLOOR | CEIL
                    | ADD | SUB | DIV | MUL | NEG | FLOAT
                    | LT_F | LE_F | GT_F | GE_F | EQ_F
                    | LT | LE | GT | GE | EQ
                    | LT_C | LE_C | GT_C | GE_C | EQ_C
                    | AND | OR | NOT
                    deriving (Eq, Show)

data AMInstruction  = LOAD_R AMRegister
                    | STORE_R AMRegister

                    | JUMP AMLabel
                    | JUMP_S
                    | JUMP_C AMLabel
                    | JUMP_O

                    | HALT

                    | LOAD_F Float
                    | LOAD_I Int
                    | LOAD_C Char
                    | LOAD_B Bool
                    | LOAD_O Int
                    | LOAD_OS

                    | STORE_O Int
                    | STORE_OS

                    | ALLOC Int
                    | ALLOC_S
                    | APP AMOperation

                    | READ_F
                    | READ_I
                    | READ_B
                    | READ_C
                    | PRINT_F
                    | PRINT_I
                    | PRINT_B
                    | PRINT_C

                    | LABEL String
                    deriving (Eq, Show)

-- data AMLine = Label AMLabel | Instruction AMInstruction deriving (Eq)
data AMLine = AMLine AMInstruction (Maybe String) deriving (Eq)

instance Show AMLine where
    -- show (Label l) = l ++ ":"
    -- show (Instruction i) = "    " ++ show i
    show (AMLine instruction comment) = case comment of
        Just c -> padRight 20 strInstruction ++ "% " ++ c
        Nothing -> strInstruction
        where
            strInstruction = case instruction of
                LABEL str -> str ++ ":"
                JUMP str -> indent ++ "JUMP " ++ str
                JUMP_C str -> indent ++ "JUMP_C " ++ str
                LOAD_F f -> indent ++ "LOAD_F " ++ show f
                LOAD_I i -> indent ++ "LOAD_I " ++ show i
                LOAD_C c -> indent ++ "LOAD_C " ++ ('"':c:'"':"")
                LOAD_B b -> indent ++ "LOAD_B " ++ if b then "true" else "false"
                LOAD_O i -> indent ++ "LOAD_O " ++ show i
                STORE_O i ->  indent ++ "STORE_O " ++ show i
                ALLOC i -> indent ++ "ALLOC " ++ show i
                _ -> indent ++ show instruction
            indent = "    "


newtype AM = AM [AMLine] deriving (Eq)

instance Show AM where
    show (AM ls) = unlines . map show $ ls

instance TargetCode AM where
    encodeToFile code h = hPutStrLn h (show code) where

type AMGenerator a m = CompilerMonadT a m

-- generate AM code from MIL
generateAMCode :: Monad m => Mil -> AMGenerator AM m
generateAMCode mil@(Mil functions) = do
    code <- mapM fromFunction functions
    let progHeader = AMLine (LOAD_R AMStackPointer) (Just "Push SOMETHING to the first slot on the stack")
    return . AM $ progHeader : concat code

-- generate AM code for a function
fromFunction :: Monad m => Function -> AMGenerator [AMLine] m
fromFunction fun@(Function label retType paramTypes localTypes body) = do
    body' <- mapM fromBasicBlock body
    let allocLocals =
            [ AMLine (LOAD_R AMStackPointer) (Just "Setting frame pointer to current frame")
            , AMLine (STORE_R AMFramePointer) Nothing
            , AMLine (ALLOC n) (Just ("Allocation " ++ show n ++ " stack frames (local variables)"))
            ]
        freeLocals = [AMLine (ALLOC (-n)) (Just ("Freeing " ++ show n ++ " stack frames (local variables)"))]
        n = length localTypes
        ret = if label == "main__"
                then [AMLine (HALT) (Just "Ending program execution")]
                else [AMLine (JUMP_S) (Just "Retruning to caller")]
    return (AMLine (LABEL label) Nothing: allocLocals ++ concat body' ++ freeLocals ++ ret)

-- generate AM code for a basic block
fromBasicBlock :: Monad m => BasicBlock -> AMGenerator [AMLine] m
fromBasicBlock bb@(BasicBlock bbid opcodes terminator) = do
    ops <- mapM fromOpCode opcodes
    term <- fromTerminator terminator
    return (AMLine (LABEL (bbIdAsLabel bbid)) Nothing: concat ops ++ term)

-- generate AM code for a MIL opcode
fromOpCode :: Monad m => OpCode -> AMGenerator [AMLine] m
fromOpCode opcode = case opcode of
    Read sym@(StackLocal{}) -> do
        op <- case symbolType sym of
            I32 -> return (AMLine READ_I Nothing)
            F32 -> return (AMLine READ_F Nothing)
            Char -> return (AMLine READ_C Nothing)
            Bool -> return (AMLine READ_B Nothing)
            t -> codegenError ("Read opcode for type " ++ show t ++ " is not support on the AM platform")
        frameCalc <- genFrameCalculation sym
        let storeVar = [AMLine (STORE_O (frameOffset sym)) (Just ("Storing to variable " ++ show (symbolName sym)))]
        return $ op:frameCalc ++ storeVar
    Print t val -> do
        valCalc <- fromMilValue val
        op <- case t of
            I32 -> return [AMLine PRINT_I Nothing]
            F32 -> return [AMLine PRINT_F Nothing]
            Char -> return [AMLine PRINT_C Nothing]
            Bool -> return [AMLine PRINT_B Nothing]
            _ -> codegenError ("Print opcode for type " ++ show t ++ " is not support on the AM platform")
        return $ valCalc ++ op
    Store sym@(StackLocal{}) val -> do
        valCalc <- fromMilValue val
        frameCalc <- genFrameCalculation sym
        let storeVar = [AMLine (STORE_O (frameOffset sym)) (Just ("Storing to variable " ++ show (symbolName sym)))]
        return $ valCalc ++ frameCalc ++ storeVar
    AllocateSlots ts -> return [AMLine (ALLOC n) (Just ("Allocation " ++ show n ++ " stack frames (local variables)"))] where
        n = length ts
    ReleaseSlots ts -> return [AMLine (ALLOC (-n)) (Just ("Freeing " ++ show n ++ " stack frames (local variables)"))] where
        n = length ts
    PushBlock -> return
        [ AMLine (LOAD_R AMFramePointer) (Just ("Pushing stack frame for M block"))
        , AMLine (ALLOC 2) Nothing
        , AMLine (LOAD_R AMStackPointer) Nothing
        , AMLine (STORE_R AMFramePointer) Nothing
        ]
    PopBlock -> return
        [ AMLine (ALLOC (-2)) (Just ("Poping stack frame for M block"))
        , AMLine (STORE_R AMFramePointer) Nothing
        ]
    _ -> codegenError $ ("Unimplemented operation: " ++ show opcode)

-- generate AM code for a basic block terminator
fromTerminator :: Monad m => Terminator -> AMGenerator [AMLine] m
fromTerminator terminator = case terminator of
    Jump label -> return [AMLine (JUMP (bbIdAsLabel label)) Nothing]
    BranchZero val label -> do
        valCalc <- fromMilValue val
        let brz = [AMLine (JUMP_C (bbIdAsLabel label)) Nothing]
        return (valCalc ++ brz)
    Branch val label -> do
        valCalc <- fromMilValue val
        let brc = [AMLine (APP NOT) Nothing, AMLine (JUMP_C (bbIdAsLabel label)) Nothing]
        return (valCalc ++ brc)
    Return (Just (_,val)) -> do
        valCalc <- fromMilValue val
        let saveRet =
                [ AMLine (LOAD_R AMFramePointer) (Just "Saving return value")
                , AMLine (STORE_O (-3)) (Just "")
                ]
        return (valCalc ++ saveRet)
    Exit Nothing -> return [AMLine (HALT) Nothing]
    Fallthrough -> return []
    _ -> codegenError $ ("Unimplemented operation: " ++ show terminator)

-- generate AM code for a MIL value
fromMilValue :: Monad m => MilValue -> AMGenerator [AMLine] m
fromMilValue val = case val of
    BinaryOp _ op lhs rhs -> do
        lhs' <- fromMilValue lhs
        rhs' <- fromMilValue rhs
        op' <- case (milTypeOf lhs, op) of
            (I32, AddOp) -> return ADD
            (I32, SubOp) -> return SUB
            (I32, MulOp) -> return MUL
            (I32, DivOp) -> return DIV

            (F32, AddOp) -> return ADD_F
            (F32, SubOp) -> return SUB_F
            (F32, MulOp) -> return MUL_F
            (F32, DivOp) -> return DIV_F

            (I32, EQOp) -> return AMCodeGenerator.EQ
            (I32, LTOp) -> return AMCodeGenerator.LT
            (I32, LEOp) -> return AMCodeGenerator.LE
            (I32, GTOp) -> return AMCodeGenerator.GT
            (I32, GEOp) -> return AMCodeGenerator.GE

            (F32, EQOp) -> return EQ_F
            (F32, LTOp) -> return LT_F
            (F32, LEOp) -> return LE_F
            (F32, GTOp) -> return GT_F
            (F32, GEOp) -> return GE_F

            (Char, EQOp) -> return EQ_C
            (Char, LTOp) -> return LT_C
            (Char, LEOp) -> return LE_C
            (Char, GTOp) -> return GT_C
            (Char, GEOp) -> return GE_C

            (Bool, AndOp) -> return AND
            (Bool, OrOp) -> return OR

            (t, _) -> codegenError $ concat ["Operation ", show op, " for type ", show t, " is not supported on the AM platform"]
        return (lhs' ++ rhs' ++ [AMLine (APP op') Nothing])
    UnaryOp _ op val -> do
        val' <- fromMilValue val
        op' <- case (milTypeOf val, op) of
            (I32, NegativeOp) -> return NEG
            (I32, FloatOp) -> return FLOAT

            (F32, NegativeOp) -> return NEG_F
            (F32, FloorOp) -> return FLOOR
            (F32, CeilingOp) -> return CEIL

            (Bool, BooleanNotOp) -> return NOT

            (t, _) -> codegenError $ concat ["Operation ", show op, " for type ", show t, " is not supported on the AM platform"]
        return (val' ++ [AMLine (APP op') Nothing])
    ConstI32 i -> return [AMLine (LOAD_I i) Nothing]
    ConstF32 f -> return [AMLine (LOAD_F f) Nothing]
    ConstChar c -> return [AMLine (LOAD_C c) Nothing]
    ConstBool b -> return [AMLine (LOAD_B b) Nothing]
    Load sym -> do
        frameCalc <- genFrameCalculation sym
        let varLoad = [AMLine (LOAD_O (frameOffset sym)) (Just ("Loading variable " ++ show (symbolName sym)))]
        return $ frameCalc ++ varLoad
    Call sym@(FunctionLabel{}) args -> do
        argsCalc <- mapM fromMilValue args
        linkCalc <- genFrameCalculation sym
        let allocRetSlot = AMLine (ALLOC 1) (Just "Allocating stack slot for return value")
            doCall =
                [ AMLine (LOAD_R AMFramePointer) (Just "Saving frame pointer")
                , AMLine (LOAD_R AMCodePointer) (Just "Saving return code")
                , AMLine (JUMP (symbolName sym)) (Just "Calling function")
                , AMLine (STORE_R AMFramePointer) (Just "Resetting frame pointer")
                , AMLine (ALLOC (-1)) (Just "Popping static link pointer")
                ]
                ++
                if null args then [] else
                    [ AMLine (LOAD_R AMStackPointer) (Just "Saving returned value")
                    , AMLine (STORE_O (- length args)) Nothing
                    ]
                ++
                if length args > 1
                    then [AMLine (ALLOC (1 - length args)) (Just "Popping arguments from function call")]
                    else []
            -- still need to recover return value
        return (concat argsCalc ++ [allocRetSlot] ++ linkCalc ++ doCall)
    _ -> codegenError $ ("Unimplemented operation: " ++ show val)

genFrameCalculation :: Monad m => Symbol -> AMGenerator [AMLine] m
genFrameCalculation sym =
    return $ AMLine (LOAD_R AMFramePointer) (Just ("Calculating stack frame of " ++ show (symbolName sym))) : chaseLinkPointer (staticLink sym)
    where
        -- generate code to chase static link pointer n times
        chaseLinkPointer :: Int -> [AMLine]
        chaseLinkPointer 0 = []
        chaseLinkPointer n = AMLine (LOAD_O (-2)) (Just "`-| chasing static link pointer"): chaseLinkPointer (n - 1)

bbIdAsLabel :: BlockId -> String
bbIdAsLabel bbid = "label_" ++ show bbid

-- emit a code generation (internal) error
codegenError :: Monad m => String -> AMGenerator a m
codegenError msg = logError ("Internal error: " ++ msg)
