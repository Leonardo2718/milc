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

{- this module defines an interface for encoding generated target code (e.g. to a file)-}

module MEncoder where

import CompilerEnvironment

import System.IO
import System.FilePath.Posix

-- type class defining a generic interface for encoding generated target code
class TargetCode c where
    writeEncodeTargetCode :: CompilerEnvironment -> c -> CompilerIOMonad ()
    writeEncodeTargetCode env code = if envSourceFile env == ""
        then liftIO . encodeToFile code $ stdout
        else do
            let outFile = replaceExtension (if envOutDir env == "" then (envSourceFile env) else replaceDirectory (envSourceFile env) (envOutDir env) ) "csh"
            liftIO . withFile outFile WriteMode . encodeToFile $ code

    encodeToFile :: c -> Handle -> IO ()
