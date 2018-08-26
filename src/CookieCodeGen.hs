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

{- This module defines the code generator for the Cookie VM -}

module CookieCodeGen (CKIE, CKIEGenerator, generateCKIE) where

-- local mild imports
import CompilerEnvironment
import MilcUtils
import MIL
import MEncoder

-- package imports
import Control.Monad.Identity

import System.IO

-- type Label = String
newtype Label = Label String deriving (Eq)

instance Show Label where
    show (Label l) = l

data Register = CKIEStackPointer | CKIEFramePointer | CKIEProgramCounter | CKIER Int deriving (Eq)

instance Show Register where
    show CKIEStackPointer = "$sp"
    show CKIEFramePointer = "$fp"
    show CKIEProgramCounter = "$pc"
    show (CKIER i) = "$" ++ show i

-- representation of Cookie type
data Type   = I32_T
            | F32_T
            | Char_T
            | Bool_T
            | IPtr_T
            | SPtr_T
            | Void_T
            deriving (Eq)

instance Show Type where
    show t = case t of
        I32_T -> "I32"
        F32_T -> "F32"
        Char_T -> "Char"
        Bool_T -> "Bool"
        IPtr_T -> "IPtr"
        SPtr_T -> "SPtr"
        Void_T -> "Void"

-- representation of Cookie Values
data Value  = I32 Int
            | F32 Float
            | Char Char
            | Bool Bool
            | IPtr Int
            | SPtr Int
            | Void
            deriving (Eq)

instance Show Value where
    show v = case v of
        CookieCodeGen.I32 val -> "I32(" ++ show val ++ ")"
        CookieCodeGen.F32 val -> "F32(" ++ show val ++ ")"
        CookieCodeGen.Char val -> "Char(" ++ show val ++ ")"
        CookieCodeGen.Bool val -> "Bool(" ++ show val ++ ")"
        IPtr val -> "IPtr(" ++ show val ++ ")"
        SPtr val -> "SPtr(" ++ show val ++ ")"
        Void -> "Void"

-- representation of Cookie v-instructions
--
-- V-Instructions are special instructions that can operate either
-- on registers or on the stack. In MILC, only stack instructions
-- are used.
data VInstruction   = ADD | SUB | DIV | MUL | MOD
                    | EQ | LT | LE | GT | GE
                    | AND | OR | XOR
                    | NEG | NOT
                    | CVT Type
                    | LOADFROM | STORETO
                    | DJUMP
                    | BRANCH Label
                    | PRINT | READ Type
                    deriving (Eq, Show)

-- representation of the core Cookie instructions
data Instruction    = PUSHR Register | PUSHC Value | POPR Register | POP
                    | Stack VInstruction
                    | JUMP Label
                    | EXIT
                    | LABEL Label
                    deriving (Eq)

-- type representing an instruction with a possibly accompanying comment
--
-- Comments can be used for debug purposes.
data CKIELine = CKIELine Instruction (Maybe String) deriving (Eq)

instance Show CKIELine where
    show (CKIELine inst comment) = case comment of
        Just c -> padRight 20 strInst ++ "; " ++ c
        Nothing -> strInst
        where
            strInst = case inst of
                LABEL l -> show l ++ ":"
                PUSHR r -> indent ++ "PUSHR " ++ show r
                PUSHC v -> indent ++ "PUSHC " ++ show v
                POPR r -> indent ++ "POPR " ++ show r
                POP -> indent ++ "POP"
                JUMP l -> indent ++ "JUMP " ++ show l
                EXIT -> indent ++ "EXIT"
                Stack (BRANCH l) -> indent ++ "BRANCH.EQ! " ++ show l
                Stack (READ t) -> indent ++ "READ " ++ show t ++ " !"
                Stack i -> indent ++ show i ++ "!"
            indent = "    "

showCKIELines :: [CKIELine] -> String
showCKIELines = unlines . map show

logCKIELines :: Monad m => [CKIELine] -> CKIEGenerator () m
logCKIELines = logBlock . showCKIELines

-- type for bundling Cookie instructions and comments
newtype CKIE = CKIE [CKIELine] deriving (Eq)

instance Show CKIE where
    show (CKIE ls) = showCKIELines ls

-- allow encoding for CKIE file
instance TargetCode CKIE where
    encodeToFile code h = hPutStrLn h (show code)

-- the main code gen monad
type CKIEGenerator a m = CompilerMonadT a m

-- generate Cookie code from MIL
generateCKIE :: Monad m => Mil -> CKIEGenerator CKIE m
generateCKIE mil@(Mil functions) = do
    logMsgLn "=== Running code generation (target: Cookie VM) ==="
    code <- mapM fromFunction functions
    -- let code = []
    logMsgLn "=== Code generation completed successfully ==="
    return . CKIE $ concat code

fromFunction :: Monad m => Function -> CKIEGenerator [CKIELine] m
fromFunction fun@(Function label retType paramTypes localTypes body) = do
    logMsgLn ("Generating code for function " ++ show label)
    allocLocals <- genStackAlloc . length $ localTypes
    releaseLocals <- genStackRelease . length $ localTypes
    let prologue = (if ismain then [] else setFP) ++ allocLocals
        epilogue = if ismain
            then releaseLocals ++ [CKIELine (EXIT) (Just "Ending program execution")]
            else saveRetVal ++ releaseLocals ++ ret
        setFP =
            [ CKIELine (PUSHR CKIEStackPointer) (Just "Setting new frame pointer")
            , CKIELine (POPR CKIEFramePointer) (Just "^")
            ]
        saveRetVal =
            [ CKIELine (PUSHR CKIEFramePointer) (Just "Saving return value")
            , CKIELine (PUSHC (CookieCodeGen.I32 (-(3+length paramTypes)))) (Just "|")
            , CKIELine (Stack ADD) (Just "|")
            , CKIELine (Stack STORETO) (Just "^")
            ]
        ret =
            [ CKIELine (PUSHC (CookieCodeGen.I32 2)) (Just "Jumping to return address")
            , CKIELine (Stack ADD) (Just "|")
            , CKIELine (Stack DJUMP) (Just "^")
            ]
        ismain = label == "main__"
    body' <- mapM (fromBasicBlock epilogue) body
    let code = CKIELine (LABEL (Label label)) Nothing : prologue ++ concat body' ++ if label == "main__" then epilogue else []
    logMsgLn ("Generated code for " ++ show label ++ " is:")
    logCKIELines code
    return code

fromBasicBlock :: Monad m => [CKIELine] -> BasicBlock -> CKIEGenerator [CKIELine] m
fromBasicBlock epilogue bb@(BasicBlock bbid opcodes terminator) = do
    logMsgLn "Generating code for basic block:"
    logBlock (show bb)
    ops <- mapM fromOpCode opcodes
    term <- fromTerminator epilogue terminator
    let code = CKIELine (LABEL (bbIdAsLabel bbid)) Nothing : concat ops ++ term
    logMsgLn "Generated code for basic block is:"
    logCKIELines code
    return code

fromOpCode :: Monad m => OpCode -> CKIEGenerator [CKIELine] m
fromOpCode opcode = do
    logMsgLn ("-- generating code for opcode: " ++ show opcode)
    code <- case opcode of
        Read sym@(StackLocal name t offset _) -> do
            frameCalc <- genFrameCalculation sym
            readType <- toCookieType t
            return  $  [CKIELine (Stack (READ readType)) Nothing]
                    ++ frameCalc
                    ++ [ CKIELine (PUSHC (CookieCodeGen.I32 offset)) (Just ("Calculating frame postion for " ++ show name))
                       , CKIELine (Stack ADD) (Just "")
                       , CKIELine (Stack STORETO) (Just ("Storing stack value to variable " ++ show name))
                       ]
        Print _ val -> do
            valCalc <- fromMilValue val
            return $ valCalc ++ [CKIELine (Stack PRINT) Nothing]
        Store sym@(StackLocal name _ offset _) val -> do
            valCalc <- fromMilValue val
            frameCalc <- genFrameCalculation sym
            let store =
                    [ CKIELine (PUSHC (CookieCodeGen.I32 offset)) (Just ("Calculating frame postion for " ++ show name))
                    , CKIELine (Stack ADD) (Just "")
                    , CKIELine (Stack STORETO) (Just ("Storing stack value to variable " ++ show name))
                    ]
            return $ valCalc ++ frameCalc ++ store
        AllocateSlots ts -> genStackAlloc . length $ ts
        ReleaseSlots ts -> genStackRelease . length $ ts
        PushBlock -> return
            [ CKIELine (PUSHR CKIEFramePointer) (Just "Pushing local block frame")
            , CKIELine (PUSHR CKIEFramePointer) Nothing
            , CKIELine (PUSHR CKIEStackPointer) Nothing
            , CKIELine (POPR CKIEFramePointer) Nothing
            ]
        PopBlock -> return
            [ CKIELine (POPR CKIEFramePointer) (Just "Poping local block frame")
            , CKIELine POP Nothing
            ]
        _ -> codegenError $ ("Unimplemented opcode: " ++ show opcode)
    logMsgLn "-- generated code for opcode:"
    logCKIELines code
    return code

fromTerminator :: Monad m => [CKIELine] -> Terminator -> CKIEGenerator [CKIELine] m
fromTerminator epilogue terminator = do
    logMsgLn ("-- generating code for terminator: " ++ show terminator)
    code <- case terminator of
        Jump target -> return [CKIELine (JUMP (bbIdAsLabel target)) Nothing]
        Branch condition target -> do
            conditionCalc <- fromMilValue condition
            return $ conditionCalc ++
                [ CKIELine (PUSHC (CookieCodeGen.Bool True)) Nothing
                , CKIELine (Stack (BRANCH (bbIdAsLabel target))) Nothing]
        BranchZero condition target -> do
            conditionCalc <- fromMilValue condition
            return $ conditionCalc ++
                [ CKIELine (PUSHC (CookieCodeGen.Bool False)) Nothing
                , CKIELine (Stack (BRANCH (bbIdAsLabel target))) Nothing
                ]
        Exit Nothing -> return $ epilogue ++ [CKIELine (EXIT) Nothing]
        Fallthrough -> return []
        Return (Just (_, val)) -> do
            valCalc <- fromMilValue val
            return $ valCalc ++ epilogue
        _ -> codegenError $ ("Unimplemented terminator: " ++ show terminator)
    logMsgLn ("-- generated code for: " ++ show terminator)
    logCKIELines code
    return code

fromMilValue :: Monad m => MilValue -> CKIEGenerator [CKIELine] m
fromMilValue val = do
    logMsgLn ("-- generating code to calculate value: " ++ show val)
    code <- case val of
        Load sym@(StackLocal name _ offset _) -> do
            frameCalc <- genFrameCalculation sym
            let load =  [ CKIELine (PUSHC (CookieCodeGen.I32 offset)) (Just ("Calculating frame postion for " ++ show name))
                        , CKIELine (Stack ADD) (Just "")
                        , CKIELine (Stack LOADFROM) (Just ("Push value of variable " ++ show name ++ " to stack"))
                        ]
            return $ frameCalc ++ load
        Call sym@(FunctionLabel name _ params _) args -> do
            argsCalc <- mapM fromMilValue args
            linkCalc <- genFrameCalculation sym
            let precall = allocRetSlot ++ concat argsCalc ++ saveFP ++ linkCalc ++ saveRetAddr
                postcall = CKIELine POP (Just "Remove static link register")
                         : CKIELine (POPR CKIEFramePointer) (Just "Restoring framepointer")
                         : removeArgs
                doCall = [CKIELine (JUMP (Label name)) (Just ("Calling function " ++ show name))]
                allocRetSlot = [CKIELine (PUSHC CookieCodeGen.Void) (Just "Allocating stack slot for return value")]
                saveFP = [CKIELine (PUSHR CKIEFramePointer) (Just "Saving current frame pointer")]
                saveRetAddr = [CKIELine (PUSHR CKIEProgramCounter) (Just "Saving return code")]
                removeArgs = if length params < 3
                    then take (length params) (repeat (CKIELine POP (Just "Popping stale argument")))
                    else [ CKIELine (PUSHR CKIEStackPointer) (Just "Removing stale arguments from stack")
                         , CKIELine (PUSHC (CookieCodeGen.I32 (- (length params)))) (Just "|")
                         , CKIELine (Stack ADD) (Just "|")
                         , CKIELine (POPR CKIEStackPointer) (Just "^")
                         ]
            return $ precall ++ doCall ++ postcall
        ConstI32 val -> return [CKIELine (PUSHC (CookieCodeGen.I32 val)) Nothing]
        ConstF32 val -> return [CKIELine (PUSHC (CookieCodeGen.F32 val)) Nothing]
        ConstChar val -> return [CKIELine (PUSHC (CookieCodeGen.Char val)) Nothing]
        ConstBool val -> return [CKIELine (PUSHC (CookieCodeGen.Bool val)) Nothing]
        BinaryOp _ op lhs rhs -> do
            lhs' <- fromMilValue lhs
            rhs' <- fromMilValue rhs
            op' <- return . Stack $ case op of
                AddOp -> ADD
                SubOp -> SUB
                MulOp -> MUL
                DivOp -> DIV
                EQOp -> CookieCodeGen.EQ
                LTOp -> CookieCodeGen.LT
                LEOp -> LE
                GTOp -> CookieCodeGen.GT
                GEOp -> GE
                AndOp -> AND
                OrOp -> OR
            return (lhs' ++ rhs' ++ [CKIELine op' Nothing])
        UnaryOp _ op val -> do
            val' <- fromMilValue val
            op' <- case op of
                NegativeOp -> return $ Stack NEG
                BooleanNotOp -> return $ Stack NOT
                FloatOp -> return $ Stack (CVT F32_T)
                _ -> codegenError $ ("Unimplemented unary operation: " ++ show op)
            return (val' ++ [CKIELine op' Nothing])
        _ -> codegenError $ ("Unimplemented operation: " ++ show val)
    logMsgLn "-- generated code is:"
    logCKIELines code
    return code

genStackAlloc :: Monad m => Int -> CKIEGenerator [CKIELine] m
genStackAlloc 0 = return [] -- allocating no stack slots requires no code
genStackAlloc n = return
    [ CKIELine (PUSHR CKIEStackPointer) (Just ("Allocation " ++ show n ++ " stack slots"))
    , CKIELine (PUSHC (CookieCodeGen.I32 n)) (Just "|")
    , CKIELine (Stack ADD) (Just "|")
    , CKIELine (POPR CKIEStackPointer) (Just "^")
    ]

genStackRelease :: Monad m => Int -> CKIEGenerator [CKIELine] m
genStackRelease 0 = return []  -- freeing no stack slots requires no code
genStackRelease n = return
    [ CKIELine (PUSHR CKIEStackPointer) (Just ("Freeing " ++ show n ++ " stack slots"))
    , CKIELine (PUSHC (CookieCodeGen.I32 (-n))) (Just "|")
    , CKIELine (Stack ADD) (Just "|")
    , CKIELine (POPR CKIEStackPointer) (Just "^")
    ]

genFrameCalculation :: Monad m => Symbol -> CKIEGenerator [CKIELine] m
genFrameCalculation sym = return $
    CKIELine (PUSHR CKIEFramePointer) (Just ("Calculating stack frame for " ++ (show . symbolName) sym)) : chaseLinkPointer (staticLink sym)
    where
        chaseLinkPointer :: Int -> [CKIELine]
        chaseLinkPointer 0 = []
        chaseLinkPointer n = CKIELine (PUSHC (CookieCodeGen.I32 (-1))) (Just "`-| chasing static link pointer")
                           : CKIELine (Stack ADD) (Just "  |")
                           : CKIELine (Stack LOADFROM) (Just "  |")
                           : chaseLinkPointer (n - 1)

bbIdAsLabel :: BlockId -> Label
bbIdAsLabel bbid = Label ("label_" ++ show bbid)

toCookieType :: Monad m => MilType -> CKIEGenerator Type m
toCookieType t = case t of
    MIL.I32 -> return I32_T
    MIL.F32 -> return F32_T
    MIL.Char -> return Char_T
    MIL.Bool -> return Bool_T
    StackPointer -> return SPtr_T
    _ -> codegenError $ "No Cookie VM type equivalent to MIL type " ++ show t

codegenError :: Monad m => String -> CKIEGenerator a m
codegenError msg = logError ("Code generation error: " ++ msg)
