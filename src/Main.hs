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

module Main where

-- system imports
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO

-- local imports
import MLexer

-- option processing -----------------------------------------------------------

-- data type for representing program (comand line) options
data Options = Options
    { optInFiles        :: [String] -- list of source files to compile
    , optStdIn          :: Bool     -- read source from std input
    , optOutFile        :: String   -- path to output file
    , optHelp           :: Bool     -- display usage information
    }

-- initializer for options
defaultOptions :: Options
defaultOptions = Options
    { optInFiles = []
    , optStdIn = False
    , optOutFile = ""
    , optHelp = False
    }

-- option transformation definition (changes option values based on command line arguments)
optionTransforms :: [OptDescr (Options -> Options)]
optionTransforms =
    [ Option    [] ["stdin"]
                (NoArg (\ opts -> opts {optStdIn = True}) )
                "read source from standard input"
    , Option    ['o'] ["output"]
                (ReqArg (\ s opts -> opts {optOutFile = s}) "FILE")
                "write output to specified file (defaults to standard output if not specified)"
    , Option    ['h'] ["help"]
                (NoArg (\ opts -> opts {optHelp = True}) )
                "display this help message"
    ]

-- option transformation application
applyOptionTransforms :: [OptDescr (Options -> Options)] -> [String] -> Options -> Options
applyOptionTransforms transforms argv defaults =
    case getOpt' Permute transforms argv of
        (o, p, [], [])  -> (foldl (flip id) defaults o) {optInFiles = p}
        (_, _, n, [])   -> error (concatMap (\ a -> "Unknown argument: " ++ a ++ "\n") n ++ helpMessage)
        (_,_,_,errors)  -> error (concat errors ++ helpMessage)

-- help message for the application
helpMessage :: String
helpMessage = usageInfo "Usage: mcomp [OPTIONS ...] [source_files ...]\n\n\
                        \  Will compile the source Minisculus files. If no source files are provided,\n\
                        \  contents of standard input will be compiled, upto EOF (Ctrl+D).\n\n\
                        \Options:" optionTransforms

-- main program ----------------------------------------------------------------

-- run the different stages of the compiler
compile :: String -> IO ()
compile = genOutput . scan where
    genOutput (Right ts) = print ts
    genOutput (Left s) = putStrLn "COMPILATION ERROR:" >> putStrLn s

-- compile a file (argument is path to the file)
compileFile :: String -> IO ()
compileFile f = readFile f >>= compile

-- invoke function based on parsed command line options
runCompiler :: Options -> IO ()
runCompiler options
    | optHelp options           = putStrLn helpMessage      -- print help message if options '-h' or '--help' where passed
    | optInFiles options == []  = getContents >>= compile   -- compile standard input if no source files were provided
    | otherwise                 = mconcat (map compileFile $ optInFiles options)  -- compile all source files

main :: IO ()
main = do
    args <- getArgs
    let options = applyOptionTransforms optionTransforms args defaultOptions
    runCompiler options
