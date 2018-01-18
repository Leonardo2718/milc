-- Copyright (C) 2016 Leonardo Banderali
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

{
module MLexer where

import Control.Monad
}

%wrapper "monadUserState"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

@identirier = $alpha[$digit $alpha]*

tokens :-
            $white+     ;
            "%".*       ;
<0>         "/*"        { startComment `andBegin` comment }
<comment>   "/*"        { embedComment }
<comment>   "*/"        { unembedComment }
<comment>   .           ;
<comment>   \n          { skip }
<0>         "*/"        { \_ _ -> alexError "Illegal */" }
<0>         "if"        { \(pos, prevc, rest, str) len -> return IF }
<0>         "then"      { \(pos, prevc, rest, str) len -> return THEN }
<0>         "else"      { \(pos, prevc, rest, str) len -> return ELSE }
<0>         "while"     { \(pos, prevc, rest, str) len -> return WHILE }
<0>         "do"        { \(pos, prevc, rest, str) len -> return DO }
<0>         "begin"     { \(pos, prevc, rest, str) len -> return BEGIN }
<0>         "end"       { \(pos, prevc, rest, str) len -> return END }
<0>         "input"     { \(pos, prevc, rest, str) len -> return INPUT }
<0>         "write"     { \(pos, prevc, rest, str) len -> return WRITE }
<0>         @identirier { \(pos, prevc, rest, str) len -> return $ ID $ take len str }
<0>         $digit+     { \(pos, prevc, rest, str) len -> return $ NUM (read (take len str) :: Int) }
<0>         ":="        { \(pos, prevc, rest, str) len -> return ASSIGN }
<0>         "+"         { \(pos, prevc, rest, str) len -> return ADD }
<0>         "-"         { \(pos, prevc, rest, str) len -> return SUB }
<0>         "*"         { \(pos, prevc, rest, str) len -> return MUL }
<0>         "/"         { \(pos, prevc, rest, str) len -> return DIV }
<0>         "("         { \(pos, prevc, rest, str) len -> return LPAR }
<0>         ")"         { \(pos, prevc, rest, str) len -> return RPAR }
<0>         ";"         { \(pos, prevc, rest, str) len -> return SEMICOLON }

{

data AlexUserState = AlexUserState { commentDepth :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { commentDepth  = 0 }

startComment input len =
    do setLexerCommentDepth 1
       skip input len

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, commentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){commentDepth=ss}}, ())

embedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip input len
unembedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode 0)
       skip input len

alexEOF :: Alex Token
alexEOF = return EOF

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

scan :: String -> Either String [Token]
scan str = runAlex str $ do
    let loop l = do
            tok <- alexMonadScan;
            if tok == EOF then do
                    commentDepth <- getLexerCommentDepth
                    if commentDepth == 0 then return l else alexError "Missing */"
            else do loop $! (l ++ [tok])
    loop []

}
