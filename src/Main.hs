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
import Data.List
import Data.Foldable
import Control.Monad
import System.FilePath.Posix

-- local imports
import CompilerEnvironment
import MLexer
import MRDParser
import MIL
import RSMGenerator

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
    [ Option    [] ["stdout"]
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
                "generate log and write it to file FILE_NAME (mcomp.log if none specified)"
    , Option    ['h'] ["help"]
                (NoArg (\ opts -> opts {optHelp = True}) )
                "display this help message"
    ] where
        getLogFile (Just f) = f
        getLogFile Nothing  = "mcomp.log"

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
helpMessage = usageInfo "Usage: mcomp [OPTIONS ...] [source_files ...]\n\n\
                        \  Will compile the source Minisculus files. If no source files are provided,\n\
                        \  contents of standard input will be compiled, upto EOF (Ctrl+D).\n\n\
                        \Options:" optionTransforms

-- main program ----------------------------------------------------------------

-- define steps of a compilation
doCompilation :: CompilerEnvironment -> CompilerMonad RSMCode
doCompilation env = do
    logMsgLn $ concat ["======= Compiling ", source , " ======="]
    ts <- scan env . envSource $ env
    (ast, _) <- parse env ts
    (mil, _) <- generateMil ast
    targetCode <- generateRSMCode mil
    logMsgLn "\nCOMPILATION SUCCEEDED!\n"
    return targetCode
    where
        source = case envSourceFile env of
            "" -> "standard input"
            f -> f

-- run the different stages of the compiler (currently only lexer)
compile :: CompilerEnvironment -> CompilerMonad RSMCode
compile env = (doCompilation env) `catchCompError` handleCompError where
    handleCompError e = logError . concat $ ["\nCOMPILATION ERROR", atLocation, ":\n", e, "\n"]
    atLocation = case envSourceFile env of
        "" -> ""
        f  -> " in " ++ f

-- compile a file (argument is path to the file)
compileFile :: String -> IO (CompilerMonad RSMCode)
compileFile f = do
    s <- readFile f
    return $ compile CompilerEnvironment {envSource = s, envSourceFile = f}

-- compile multiple files, merging their compilation output together
compileFiles :: [String] -> IO [CompilerMonad RSMCode]
compileFiles = mapM compileFile

-- compile source from standard input
compileStdIn :: IO (CompilerMonad RSMCode)
compileStdIn = do
    s <- getContents
    return . compile $ CompilerEnvironment {envSource = s, envSourceFile = ""}

-- print the compilers' outputs and log
printCompilerOutputs :: Show a => Options -> [CompilerMonad a] -> IO ()
printCompilerOutputs options comps = if optStdOut options
    then mapM_ (putStrLn . extractOutput . showCompilerOutput) comps
    else mapM_ writeOutput (zip (optInFiles options) comps)
    where
        extractOutput o = case o of
            Left e -> e
            Right o' -> o'
        writeOutput (s, c) = do
            let outFile = replaceExtension (if optOutDir options == "" then s else replaceDirectory s (optOutDir options) ) "csh"
            case showCompilerOutput c of
                Left e -> putStrLn e
                Right o -> writeFile outFile o
printLogsAndOutputs :: Show a => Options -> [CompilerMonad a] -> IO ()
printLogsAndOutputs options comps = do
    printCompilerOutputs options comps
    when (optLogFile options /= "") $ writeLogFile comps
    where
        writeLogFile cs = case cs of
            [] -> return ()
            c:cs -> writeLogFileWith writeFile c >> mapM_ (writeLogFileWith appendFile) cs
        writeLogFileWith fileWriter c = fileWriter (optLogFile options) . showCompilerLog $ c

-- invoke action based on parsed command line options
runMain :: Options -> IO ()
runMain options
    | optHelp options           = putStrLn helpMessage
        -- print help message if options '-h' or '--help' where passed
    | optStdIn options          = do c <- compileStdIn; printLogsAndOutputs options [c];
        -- force compilation of standard input
    | otherwise                 = (compileFiles $ optInFiles options) >>= printLogsAndOutputs options
        -- compile all source files

main :: IO ()
main = do
    args <- getArgs
    let options = applyOptionTransforms optionTransforms args defaultOptions
    runMain options
