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

{- This module contains general utilities useful in all parts of the compiler -}

module MilcUtils where

import CompilerEnvironment

import Data.List


-- helper function for showing the first n elements of a list
--
-- If the list has more than n elements, then the first n are shown followed by
-- "...". Otherwise, the list is shown as is.
showFirst :: Show a => Int -> [a] -> String
showFirst n l = if length l > n
    then concat ["[", intercalate ", " (map show firsts), ", ..."]
    else show firsts
    where
        firsts = take n l

-- helper function for showing the first n lines of a string
--
-- If the string has more than n lines, then the first n are shown followed by
-- "...". Otherwise, the string is shown as is.
showFirstLines :: Int -> String -> String
showFirstLines n = asLines . lines where
    asLines ls = if length ls > n
        then (unlines . take n $ ls) ++ "  ..."
        else intercalate "\n" ls

-- helper for putting a log message inside a block
logBlock :: Monad m => String -> CompilerMonadT () m
logBlock msg = do
    logMsgLn "--------------------------------------------------"
    logMsgLn msg
    logMsgLn "--------------------------------------------------"

-- helper for putting the first n lines of a log message in a block
logBlockLines :: Monad m => Int -> String -> CompilerMonadT () m
logBlockLines n = logBlock . showFirstLines n

concat2WithPadding :: Int -> String -> String -> String
concat2WithPadding n a b = concat [a, padding, b] where
    padding = take (n - length a) (repeat ' ')
