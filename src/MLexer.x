--
-- Copyright (C) 2018 Leonardo Banderali
--
-- License:
--
--     Permission is hereby granted, free of charge, to any person obtaining a copy
--     of this software and associated documentation files (the "Software"), to deal
--     in the Software without restriction, including without limitation the rights
--     to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--     copies of the Software, and to permit persons to whom the Software is
--     furnished to do so, subject to the following conditions:
--
--     The above copyright notice and this permission notice shall be included in
--     all copies or substantial portions of the Software.
--
--     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--     IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--     FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--     AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--     LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--     OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--     THE SOFTWARE.
--

{
module MLexer where

import CompilerEnvironment

import Control.Monad
import Data.List
}

%wrapper "monadUserState"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$char = [_A-Za-z0-9]   -- M character

@identirier = $alpha[$digit $alpha]*
@constructor = "#"[_ $digit $alpha]*

-- multi-line comment handling largely inspired by:
-- https://github.com/simonmar/alex/blob/master/examples/tiger.x

tokens :-
            $white+     ;
            "%".*       ;
<0>         "/*"        { startComment `andBegin` comment }
<comment>   "/*"        { embedComment }
<comment>   "*/"        { unembedComment }
<comment>   .           ;
<0>         "*/"        { lexerError (\_ -> "Unexpected */") }

<0>         "+"         { emitToken (\_ -> ADD_T) }
<0>         "-"         { emitToken (\_ -> SUB_T) }
<0>         "*"         { emitToken (\_ -> MUL_T) }
<0>         "/"         { emitToken (\_ -> DIV_T) }

<0>         "=>"        { emitToken (\_ -> ARROW_T) }

<0>         "&&"        { emitToken (\_ -> AND_T) }
<0>         "||"        { emitToken (\_ -> OR_T) }
<0>         "not"       { emitToken (\_ -> NOT_T) }

<0>         "="         { emitToken (\_ -> EQ_T) }
<0>         "<"         { emitToken (\_ -> LT_T) }
<0>         "=<"        { emitToken (\_ -> LE_T) }
<0>         ">"         { emitToken (\_ -> GT_T) }
<0>         ">="        { emitToken (\_ -> GE_T) }

<0>         ":="        { emitToken (\_ -> ASSIGN_T) }

<0>         "("         { emitToken (\_ -> LPAREN_T) }
<0>         ")"         { emitToken (\_ -> RPAREN_T) }
<0>         "{"         { emitToken (\_ -> LBRACE_T) }
<0>         "}"         { emitToken (\_ -> RBRACE_T) }
<0>         "["         { emitToken (\_ -> LBRACK_T) }
<0>         "]"         { emitToken (\_ -> RBRACK_T) }

<0>         "|"         { emitToken (\_ -> PIPE_T) }

<0>         ":"         { emitToken (\_ -> COLON_T) }
<0>         ";"         { emitToken (\_ -> SEMICOLON_T) }
<0>         ","         { emitToken (\_ -> COMMA_T) }

<0>         "if"        { emitToken (\_ -> IF_T) }
<0>         "then"      { emitToken (\_ -> THEN_T) }
<0>         "else"      { emitToken (\_ -> ELSE_T) }
<0>         "while"     { emitToken (\_ -> WHILE_T) }
<0>         "do"        { emitToken (\_ -> DO_T) }
<0>         "case"      { emitToken (\_ -> CASE_T) }
<0>         "of"        { emitToken (\_ -> OF_T) }

<0>         "read"      { emitToken (\_ -> READ_T) }
<0>         "print"     { emitToken (\_ -> PRINT_T) }
<0>         "floor"     { emitToken (\_ -> FLOOR_T) }
<0>         "ceil"      { emitToken (\_ -> CEIL_T) }

<0>         "return"    { emitToken (\_ -> RETURN_T) }

<0>         "size"      { emitToken (\_ -> SIZE_T) }
<0>         "float"     { emitToken (\_ -> FLOAT_T) }

<0>         "int"       { emitToken (\_ -> INT_T) }
<0>         "real"      { emitToken (\_ -> REAL_T) }
<0>         "char"      { emitToken (\_ -> CHAR_T) }
<0>         "bool"      { emitToken (\_ -> BOOL_T) }
<0>         "var"       { emitToken (\_ -> VAR_T) }
<0>         "fun"       { emitToken (\_ -> FUN_T) }
<0>         "data"      { emitToken (\_ -> DATA_T) }

<0>         "true"              { emitToken (\_ -> BOOLVAL_T True) }
<0>         "false"             { emitToken (\_ -> BOOLVAL_T False) }
<0>         @identirier         { emitToken (\s -> ID_T s) }
<0>         @constructor        { emitToken (\s -> CTOR_T s) }
<0>         $digit+             { emitToken (\s -> INTVAL_T (read s)) }
<0>         $digit+ "." $digit+ { emitToken (\s -> REALVAL_T (read s)) }
<0>         \" $char \"         { emitToken (\s -> CHARVAL_T (s !! 1)) }-- "
<0>         \" "\n" \"          { emitToken (\s -> CHARVAL_T '\n') }    -- "
<0>         \" "\t" \"          { emitToken (\s -> CHARVAL_T '\t') }    -- "

            .           { lexerError (\s -> "Unrecognized character pattern strating with: " ++ s) }

{

-- type for storing information about the lexing environment
data LexerEnvironment = LexerEnvironment
    { lexSource      :: String   -- the actual source code being compiled
    , lexSourceFile  :: String   -- path to the file containing the source code (empty if using stdin)
    }

-- user state data type (for use with `monadUserState` wrapper)
type AlexPosnStack = [AlexPosn]
data AlexUserState = AlexUserState
    { activeCommentStarts   :: AlexPosnStack    -- stores the position of currently active (un-closed) "/*"
    , lexerEnvironment      :: LexerEnvironment -- access to current compilation Environment
    }

-- user state initializer
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState   { activeCommentStarts   = []
                                    , lexerEnvironment      = LexerEnvironment "" ""
                                    }

-- getters and setters of the user state
getActiveCommentStarts :: Alex AlexPosnStack
getActiveCommentStarts = Alex $ \ s@AlexState{alex_ust=ust} -> Right (s, activeCommentStarts ust)

setActiveCommentStarts :: AlexPosnStack -> Alex ()
setActiveCommentStarts ss = Alex $ \ s -> Right (s{alex_ust=(alex_ust s){activeCommentStarts=ss}}, ())

pushActiveCommentStart :: AlexPosn -> Alex ()
pushActiveCommentStart p = do
    ps <- getActiveCommentStarts
    setActiveCommentStarts (p:ps)

popActiveCommentStart :: Alex (AlexPosn, AlexPosnStack)
popActiveCommentStart = do
    (p:ps) <- getActiveCommentStarts
    setActiveCommentStarts ps
    return (p, ps)

getLexerEnvironment :: Alex LexerEnvironment
getLexerEnvironment = Alex $ \ s@AlexState{alex_ust=ust} -> Right (s, lexerEnvironment ust)

setLexerEnvironment :: LexerEnvironment -> Alex ()
setLexerEnvironment env = Alex $ \ s -> Right (s{alex_ust=(alex_ust s){lexerEnvironment=env}}, ())

-- helpers for multi-line comment handling
startComment :: AlexInput -> Int -> Alex Token
startComment input@(pos,_,_,_) len = do     -- setup state for comment handling
    pushActiveCommentStart pos
    skip input len

embedComment :: AlexInput -> Int -> Alex Token
embedComment input@(pos,_,_,_) len = do     -- handle a nested comment
    pushActiveCommentStart pos
    skip input len

unembedComment :: AlexInput -> Int -> Alex Token
unembedComment input len = do               -- handle close of nested comment
    (p, ps) <- popActiveCommentStart
    when (length ps == 0) (alexSetStartCode 0)
    skip input len

-- helper for printing "AlexPosn" values in a more human-friendly manner
showAlexPos :: AlexPosn -> String
showAlexPos (AlexPn _ l c) = concat ["line ", show l, ", column ", show c]

-- helper for generating lexer errors
--
-- The first argument is a function that returns a custom error message,
-- specified by the caller. It is invoked with the current lexeme as argument.
lexerError :: (String -> String) -> AlexInput -> Int -> Alex Token
lexerError mkmsg (pos,_,_,str) len = do
    env <- getLexerEnvironment
    let lexerMsg = concat ["Lexical error at ", showAlexPos pos, ": ", mkmsg (take len str), "\n"
                          , let AlexPn _ l c = pos in showErrorLocation (lexSource env) l c
                          ]
    alexError lexerMsg

-- helper for defining lexer token actions
--
-- The first argument is a function that returns the current Token instance.
-- It is invoked with the current lexeme as argument.
emitToken :: (String -> TokenType) -> AlexInput -> Int -> Alex Token
emitToken emiter (pos, prevc, rest, str) len = return $ Token (emiter (take len str)) pos

-- define the end-of-file token for Alex
alexEOF :: Alex Token
alexEOF = return EOF

-- the Token type
data TokenType  = ADD_T
                | SUB_T
                | MUL_T
                | DIV_T

                | ARROW_T

                | AND_T
                | OR_T
                | NOT_T

                | EQ_T
                | LT_T
                | LE_T
                | GT_T
                | GE_T

                | ASSIGN_T

                | LPAREN_T
                | RPAREN_T
                | LBRACE_T
                | RBRACE_T
                | LBRACK_T
                | RBRACK_T

                | PIPE_T

                | COLON_T
                | SEMICOLON_T
                | COMMA_T

                | IF_T
                | THEN_T
                | ELSE_T
                | WHILE_T
                | DO_T
                | CASE_T
                | OF_T

                | BEGIN_T
                | END_T

                -- | INPUT_T
                | READ_T
                -- | WRITE_T
                | PRINT_T
                | FLOOR_T
                | CEIL_T

                | RETURN_T

                | SIZE_T
                | FLOAT_T

                | INT_T
                | REAL_T
                | CHAR_T
                | BOOL_T
                | VAR_T
                | FUN_T
                | DATA_T

                | ID_T      { idname :: String }
                | CTOR_T    { ctor :: String }
                | INTVAL_T  { intval :: Int }
                | REALVAL_T { realval :: Float }
                | CHARVAL_T { charval :: Char }
                | BOOLVAL_T { boolval :: Bool }
                deriving (Eq, Show)

data Token  = Token {tokenType :: TokenType, tokenPos :: AlexPosn} | EOF deriving (Eq)

token_idname = idname . tokenType
token_ctor = ctor . tokenType
token_intval = intval . tokenType
token_realval = realval . tokenType
token_charval = charval . tokenType
token_boolval = boolval . tokenType

instance Show Token where
    show (Token tt pos) = concat [show tt, " (", showAlexPos pos, ")"]
    show EOF = "EOF"

-- wrapper function for alexMonadScan for generating a Token, emitting error
-- messages when necessary
scanToken :: Alex Token
scanToken = do
    tok <- alexMonadScan
    if tok == EOF then do
        activeComments <- getActiveCommentStarts
        env <- getLexerEnvironment
        case activeComments of
            []  -> return tok
            pos -> do
                let msg = concat $  [ "Missing ", show (length pos), " */, openning /* at:\n\t"
                                    , intercalate "\n\t" $ map errorPos pos, "\n"
                                    ]
                    errorPos p = showAlexPos p ++ "\n" ++ showErrorLocationL "\t" (lexSource env) l c where
                        AlexPn _ l c = p
                alexError msg
    else return tok

-- helper function for scanToken that collects tokens and puts them in a list
-- until EOF is found
collectTokens :: Alex [Token]
collectTokens = do
    let loop l = do
            tok <- scanToken
            if tok == EOF then return l else do loop $! (l ++ [tok])
    loop []

runAlexCompiler :: Monad m => LexerEnvironment -> Alex a -> CompilerMonadT a m
runAlexCompiler env a = case runAlex (lexSource env) (setLexerEnvironment env >> a) of
    Right a'    -> return a'
    Left e      -> compError e

-- helper function that scans a string and, if successful, returns a list of
-- tokens, or emits an error otherwise
scan :: Monad m => LexerEnvironment -> CompilerMonadT [Token] m
scan env = do
    logMsgLn "=== Running lexical analysis ==="
    ts <- rewrap $ runAlex (lexSource env) (setLexerEnvironment env >> collectTokens)
    logMsgLn "Lexical analysis successful"
    logMsgLn $ concat ["Tokens: ", show ts]
    return ts
    where
        rewrap (Right ts) = return ts
        rewrap (Left e)   = compError e

}
