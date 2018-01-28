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

import Control.Monad
import Data.List

import CompilerEnvironment
}

%wrapper "monadUserState"

$digit = 0-9        -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

@identirier = $alpha[$digit $alpha]*

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
<0>         "if"        { emitToken (\_ -> IF) }
<0>         "then"      { emitToken (\_ -> THEN) }
<0>         "else"      { emitToken (\_ -> ELSE) }
<0>         "while"     { emitToken (\_ -> WHILE) }
<0>         "do"        { emitToken (\_ -> DO) }
<0>         "begin"     { emitToken (\_ -> BEGIN) }
<0>         "end"       { emitToken (\_ -> END) }
<0>         "input"     { emitToken (\_ -> INPUT) }
<0>         "write"     { emitToken (\_ -> WRITE) }
<0>         @identirier { emitToken (\s -> ID s) }
<0>         $digit+     { emitToken (\s -> NUM (read s)) }
<0>         ":="        { emitToken (\_ -> ASSIGN) }
<0>         "+"         { emitToken (\_ -> ADD) }
<0>         "-"         { emitToken (\_ -> SUB) }
<0>         "*"         { emitToken (\_ -> MUL) }
<0>         "/"         { emitToken (\_ -> DIV) }
<0>         "("         { emitToken (\_ -> LPAR) }
<0>         ")"         { emitToken (\_ -> RPAR) }
<0>         ";"         { emitToken (\_ -> SEMICOLON) }
            .           { lexerError (\s -> "Unrecognized character " ++ s) }

{

-- user state data type (for use with `monadUserState` wrapper)
type AlexPosnStack = [AlexPosn]
data AlexUserState = AlexUserState
    { activeCommentStarts   :: AlexPosnStack        -- stores the position of currently active (un-closed) "/*"
    , compilerEnvironment   :: CompilerEnvironment  -- access to current compilation Environment
    }

-- user state initializer
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState   { activeCommentStarts   = []
                                    , compilerEnvironment   = CompilerEnvironment "" ""
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

getCompilerEnvironment :: Alex CompilerEnvironment
getCompilerEnvironment = Alex $ \ s@AlexState{alex_ust=ust} -> Right (s, compilerEnvironment ust)

setCompilerEnvironment :: CompilerEnvironment -> Alex ()
setCompilerEnvironment env = Alex $ \ s -> Right (s{alex_ust=(alex_ust s){compilerEnvironment=env}}, ())

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
showAlexPos (AlexPn _ l c) = concat ["line ", show l, " column ", show c]

-- helper for generating lexer errors
--
-- The first argument is a function that returns a custom error message,
-- specified by the caller. It is invoked with the current lexeme as argument.
lexerError :: (String -> String) -> AlexInput -> Int -> Alex Token
lexerError mkmsg (pos,_,_,str) len = alexError (lexerMsg ++ mkmsg (take len str)) where
    lexerMsg = concat ["Lexical error at ", showAlexPos pos, ": "]

-- helper for defining lexer token actions
--
-- The first argument is a function that returns the current Token instance.
-- It is invoked with the current lexeme as argument.
emitToken :: (String -> Token) -> AlexInput -> Int -> Alex Token
emitToken emiter (pos, prevc, rest, str) len = return (emiter (take len str))

-- define the end-of-file token for Alex
alexEOF :: Alex Token
alexEOF = return EOF

-- the Token type
data Token  = IF
            | THEN
            | ELSE
            | WHILE
            | DO
            | BEGIN
            | END
            | INPUT
            | WRITE
            | ID String
            | NUM Int
            | ASSIGN
            | ADD
            | SUB
            | MUL
            | DIV
            | LPAR
            | RPAR
            | SEMICOLON
            | EOF deriving (Eq, Show)

-- wrapper function for alexMonadScan for generating a Token, emitting error
-- messages when necessary
scanToken :: Alex Token
scanToken = do
    tok <- alexMonadScan
    if tok == EOF then do
        activeComments <- getActiveCommentStarts
        case activeComments of
            []  -> return tok
            pos -> alexError . concat $ [ "Missing ", show (length pos), " */, openning /* at:\n\t"
                                        , intercalate "\n\t" $ map showAlexPos pos ]
    else return tok

-- helper function for scanToken that collects tokens and puts them in a list
-- until EOF is found
collectTokens :: Alex [Token]
collectTokens = do
    let loop l = do
            tok <- scanToken
            if tok == EOF then return l else do loop $! (l ++ [tok])
    loop []

-- helper function that scans a string and, if successful, returns a list of
-- tokens, or emits an error otherwise
scan :: CompilerEnvironment -> String -> CompilerMonad [Token]
scan env str = do
    logMsg "=== Running lexical analysis ==="
    ts <- rewrap $ runAlex str (setCompilerEnvironment env >> collectTokens)
    logMsg "Lexical analysis succeeded"
    logMsg $ concat ["Tokens: ", show ts]
    return ts
    where
        rewrap (Right ts) = return ts
        rewrap (Left e)   = compError e

}
