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

type AlexPosnStack = [AlexPosn]
data AlexUserState = AlexUserState  { activeCommentStarts :: AlexPosnStack }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState   { activeCommentStarts = mempty }

getActiveCommentStarts :: Alex [AlexPosn]
getActiveCommentStarts = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, activeCommentStarts ust)

setActiveCommentStarts :: [AlexPosn] -> Alex ()
setActiveCommentStarts ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){activeCommentStarts=ss}}, ())

pushActiveCommentStart :: AlexPosn -> Alex ()
pushActiveCommentStart p = do
    ps <- getActiveCommentStarts
    setActiveCommentStarts (p:ps)

popActiveCommentStart :: Alex (AlexPosn, [AlexPosn])
popActiveCommentStart = do
    (p:ps) <- getActiveCommentStarts
    setActiveCommentStarts ps
    return (p, ps)

startComment :: AlexInput -> Int -> Alex Token
startComment input@(pos,_,_,_) len = do
    pushActiveCommentStart pos
    skip input len

embedComment :: AlexInput -> Int -> Alex Token
embedComment input@(pos,_,_,_) len = do
    pushActiveCommentStart pos
    skip input len

unembedComment :: AlexInput -> Int -> Alex Token
unembedComment input len = do
    (p, ps) <- popActiveCommentStart
    when (length ps == 0) (alexSetStartCode 0)
    skip input len

showAlexPos :: AlexPosn -> String
showAlexPos (AlexPn _ l c) = concat ["line ", show l, " column ", show c]

lexerError :: (String -> String) -> AlexInput -> Int -> Alex Token
lexerError mkmsg ((AlexPn _ l c),_,_,str) len = alexError (lexerMsg ++ mkmsg (take len str)) where
    lexerMsg = concat ["Lexical error at line ", show l, ", column ", show c, ": "]

alexEOF :: Alex Token
alexEOF = return EOF

emitToken :: (String -> Token) -> AlexInput -> Int -> Alex Token
emitToken emiter (pos, prevc, rest, str) len = return (emiter (take len str))

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

scanToken :: Alex Token
scanToken = do
    tok <- alexMonadScan
    if tok == EOF then do
        activeComments <- getActiveCommentStarts
        case activeComments of
            []  -> return tok
            pos -> alexError . concat $ [ "Missing ", show (length pos), " */, openning /* at:\n\t"
                                                  , intercalate "\n\t" $ map showAlexPos pos]
    else return tok

scan :: String -> Either String [Token]
scan str = runAlex str $ do
    let loop l = do
            tok <- scanToken
            if tok == EOF then return l else do loop $! (l ++ [tok])
    loop []

}
