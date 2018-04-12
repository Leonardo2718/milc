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
    logMsgLn ",-------------------------------------------------"
    logMsgLn msg
    logMsgLn "`-------------------------------------------------"

-- helper for putting the first n lines of a log message in a block
logBlockLines :: Monad m => Int -> String -> CompilerMonadT () m
logBlockLines n = logBlock . showFirstLines n

concat2WithPadding :: Int -> String -> String -> String
concat2WithPadding n a b = concat [a, padding, b] where
    padding = take (n - length a) (repeat ' ')

padRight :: Int -> String -> String
padRight n str = str ++ take (n - length str) (repeat ' ')

padLeft :: Int -> String -> String
padLeft n str = take (n - length str) (repeat ' ') ++ str

-- given a monadic action, produces an action that takes some value, performs
-- the input action, and simply returns (forwards) the input value, discarding
-- the result of the input action
forwardAnd :: Monad m => m a -> b -> m b
forwardAnd c a = c >> return a

-- composes two monad actions by discarding the result of the second and
-- forwarding the result of the first
infixl 1 =>>
(=>>) :: Monad m => m a -> m b -> m a
a =>> b = a >>= forwardAnd b

-- binds to monadic values containing lists by creating a new monadic value
-- concatinating the two lists
infixl 1 >+>
(>+>) :: Monad m => m [a] -> m [a] -> m [a]
a >+> b =  do
    l1 <- a
    l2 <- b
    return (l1 ++ l2)

addLeading :: String -> [String] -> [String]
addLeading lead strs = map (\ s -> lead ++ s) strs

-- helper for generating detailed option descriptions
optionDesc :: String -> [String] -> String
optionDesc optStr ls = intercalate "\n" desc where
    desc = [ optLead ++ head ls ] ++ addLeading descLead (tail ls)
    optLead = "  " ++ optStr ++ "  "
    descLead = take (length optLead) (repeat ' ')
