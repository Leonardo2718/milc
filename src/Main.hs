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

-- local imports
import CompilerEnvironment
import MLexer
import MRDParser
import MILGenerator
import MEncoder
import RSMGenerator

-- system imports
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO
import Data.List
import Data.Foldable
import Control.Monad


-- option processing -----------------------------------------------------------

-- data type for representing program (comand line) options
data Options = Options
    { optInFiles        :: [String] -- list of source files to compile
    , optStdOut         :: Bool     -- print ouput to standard output
    , optStdIn          :: Bool     -- read source from std input
    , optOutDir         :: String   -- path to output directory
    , optLogFile        :: String
    , optHelp           :: Bool     -- display usage information
    }

-- initializer for options
defaultOptions :: Options
defaultOptions = Options
    { optInFiles = []
    , optStdOut = False
    , optStdIn = False
    , optOutDir = ""
    , optLogFile = ""
    , optHelp = False
    }

-- option transformation definition (changes option values based on command line arguments)
optionTransforms :: [OptDescr (Options -> Options)]
optionTransforms =
    [ Option    ['h'] ["help"]
                (NoArg (\ opts -> opts {optHelp = True}) )
                "display this help message"
    , Option    [] ["stdout"]
                (NoArg (\ opts -> opts {optStdOut = True}))
                "print output to standard output"
    , Option    [] ["stdin"]
                (NoArg (\ opts -> opts {optStdIn = True, optStdOut = True}) )
                "read source from standard input (implies --stdout)"
    , Option    ['d'] ["outdir"]
                (ReqArg (\ s opts -> opts {optOutDir = s}) "DIRECTORY")
                "put output files in directory DIRECTORY"
    , Option    ['l'] ["log"]
                (OptArg (\ s opts -> opts {optLogFile = getLogFile s}) "FILE_NAME")
                "generate log and write it to file FILE_NAME (milc.log if none specified)"
    ] where
        getLogFile (Just f) = f
        getLogFile Nothing  = "milc.log"

-- option transformation application
applyOptionTransforms :: [OptDescr (Options -> Options)] -> [String] -> Options -> Options
applyOptionTransforms transforms argv defaults = opts {optStdOut=newStdOut,optStdIn=newStdIn} where
    newStdOut = optStdOut opts || (optInFiles opts == [])
    newStdIn  = optStdIn opts || (optInFiles opts == [])
    opts = case getOpt' Permute transforms argv of
        (o, p, [], [])  -> (foldl (flip id) defaults o) {optInFiles = p}
        (_, _, n, [])   -> error (concatMap (\ a -> "Unknown argument: " ++ a ++ "\n") n ++ helpMessage)
        (_,_,_,errors)  -> error (concat errors ++ helpMessage)

-- help message for the application
helpMessage :: String
helpMessage = usageInfo "Usage: milc [OPTIONS ...] [source_files ...]\n\n\
                        \  Will compile all source Minisculus files. If no source files are provided,\n\
                        \  contents of standard input will be compiled, up to EOF (Ctrl+D). By default,\n\
                        \  generated files will be put in the same directory as source files (or printed\n\
                        \  to standard output if compiling from standard input). Use the `--outdir`\n\
                        \  option to specify a different destination directory for all generated files.\n\
                        \  All compilation errors are printed to standard error. \n\n\
                        \Options:" optionTransforms

-- main program ----------------------------------------------------------------

-- define steps of a compilation
doCompilation :: CompilerEnvironment -> CompilerMonadT () IO
doCompilation env = do
    logMsgLn $ concat ["======= Compiling ", source , " ======="]
    ts <- scan env . envSource $ env
    (ast, _) <- parse env ts
    (mil, _) <- generateMil ast
    targetCode <- generateRSMCode mil
    writeEncodeTargetCode env targetCode
    logMsgLn "\nCOMPILATION SUCCEEDED!\n"
    where
        source = case envSourceFile env of
            "" -> "standard input"
            f -> f

-- compile source from standard input
compileStdIn :: CompilerMonadT () IO
compileStdIn = do
    s <- liftIO getContents
    compile CompilerEnvironment {envSource = s, envSourceFile = "", envOutDir=""}

-- run the different stages of the compiler (currently only lexer)
compile :: CompilerEnvironment -> CompilerMonadT () IO
compile env = (doCompilation env) `catchCompError` handleCompError where
    handleCompError e = do
        let msg = concat $ ["\nCOMPILATION ERROR", atLocation, ":\n", e, "\n"]
        logMsgLn msg
        liftIO $ hPutStrLn stderr msg
    atLocation = case envSourceFile env of
        "" -> ""
        f  -> " in " ++ f

-- compile a file (argument is path to the file)
compileFile :: String -> String -> CompilerMonadT () IO
compileFile outDir f = do
    s <- liftIO $ readFile f
    compile CompilerEnvironment {envSource = s, envSourceFile = f, envOutDir = outDir}

-- compile multiple files, merging their compilation output together
compileFiles :: String -> [String] -> CompilerMonadT () IO
compileFiles outDir = mapM_ $ compileFile outDir

-- invoke action based on parsed command line options
runMain :: Options -> IO ()
runMain options = if optHelp options
    then putStrLn helpMessage   -- print help message
    else do                     -- compile all source files
        let comp = if optStdIn options
            then compileStdIn
            else compileFiles (optOutDir options) (optInFiles options)
        (_, l) <- runCompilerT comp
        when (optLogFile options /= "") $ writeFile (optLogFile options) (concat l)

main :: IO ()
main = do
    args <- getArgs
    let options = applyOptionTransforms optionTransforms args defaultOptions
    runMain options
