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
import MilcUtils
import MLexer
import MParser
import MSemantics
import MIL
import MilcOptimizer
import AMCodeGenerator
import MEncoder
-- import MRDParser
-- import MILGenerator
-- import RSMGenerator

-- system imports
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO
import Data.List
import Data.Foldable
import Control.Monad


-- option processing -----------------------------------------------------------

-- data type for representing program (comand line) options
data MilcOptions = MilcOptions
    { milcInFiles        :: [String] -- list of source files to compile
    , milcStdOut         :: Bool     -- print ouput to standard output
    , milcStdIn          :: Bool     -- read source from std input
    , milcOptLevel       :: Int      -- optimization level
    , milcOutDir         :: String   -- path to output directory
    , milcLogFile        :: String
    , milcHelp           :: Bool     -- display usage information
    }

-- initializer for options
defaultMilcOptions :: MilcOptions
defaultMilcOptions = MilcOptions
    { milcInFiles = []
    , milcStdOut = False
    , milcStdIn = False
    , milcOptLevel = 1
    , milcOutDir = ""
    , milcLogFile = ""
    , milcHelp = False
    }

-- option transformation definition (changes option values based on command line arguments)
optionTransforms :: [OptDescr (MilcOptions -> MilcOptions)]
optionTransforms =
    [ Option    ['h'] ["help"]
                (NoArg (\ opts -> opts {milcHelp = True}) )
                "display this help message"
    -- , Option    [] ["stdout"]
    --             (NoArg (\ opts -> opts {milcStdOut = True}))
    --             "print output to standard output"
    , Option    [] ["stdin"]
                (NoArg (\ opts -> opts {milcStdIn = True, milcStdOut = True}) )
                "read source from standard input (implies --stdout)"
    , Option    ['O'] []
                (ReqArg (\ s opts -> opts {milcOptLevel = read s}) "LEVEL")
                ("set the optimization level to LEVEL (default is " ++ show (milcOptLevel defaultMilcOptions) ++ ")")
    , Option    ['d'] ["outdir"]
                (ReqArg (\ s opts -> opts {milcOutDir = s}) "DIRECTORY")
                "put output files in directory DIRECTORY"
    , Option    ['l'] ["log"]
                (OptArg (\ s opts -> opts {milcLogFile = getLogFile s}) "FILE_NAME")
                "generate log and write it to file FILE_NAME (milc.log if none specified)"
    ] where
        getLogFile (Just f) = f
        getLogFile Nothing  = "milc.log"

-- option transformation application
applyOptionTransforms :: [OptDescr (MilcOptions -> MilcOptions)] -> [String] -> MilcOptions -> MilcOptions
applyOptionTransforms transforms argv defaults = opts {milcStdOut=newStdOut,milcStdIn=newStdIn} where
    newStdOut = milcStdOut opts || (milcInFiles opts == [])
    newStdIn  = milcStdIn opts || (milcInFiles opts == [])
    opts = case getOpt' Permute transforms argv of
        (o, p, [], [])  -> (foldl (flip id) defaults o) {milcInFiles = p}
        (_, _, n, [])   -> error (concatMap (\ a -> "Unknown argument: " ++ a ++ "\n") n ++ helpMessage)
        (_,_,_,errors)  -> error (concat errors ++ helpMessage)

-- help message for the application
helpMessage :: String
helpMessage = msg where
    msg = unlines [usage, "", notes, optDesc, details]
    usage = "Usage: milc [OPTIONS ...] [source_files ...]"
    notes = unlines . addLeading "  " $
        [ "Will compile all M source files. If no source files are provided,"
        , "contents of standard input will be compiled, up to EOF (Ctrl+D)."
        , "By default, generated files will be put in the same directory as"
        , "source files (or printed to standard output if compiling from"
        , "standard input). Use the `--outdir` option to specify a different"
        , "destination directory for all generated files. All compilation"
        , "errors are printed to standard error."
        ]
    optDesc = (usageInfo "OPTIONS:" optionTransforms)
    details = MilcOptimizer.helpMsg

-- main program ----------------------------------------------------------------

-- IO action for getting source code
type SourceAction = CompilerMonadT String IO
sourceAction :: IO String -> SourceAction
sourceAction a = do
    s <- liftIO $ a
    return s

-- define steps of a compilation
doCompilation :: String -> SourceAction -> MilcOptions -> CompilerMonadT () IO
doCompilation f getSource options = do
    s <- getSource
    let sourceFile = if f == "" then "standard input" else f
    logMsgLn $ concat ["======= Compiling ", sourceFile , " ======="]
    t <- parse $ LexerEnvironment {lexSource = s, lexSourceFile = f}
    (_, state) <- runSemanticAnalyzer analyzeAST t (MSemanticAnalyzerEnvironment {compSource = s, compSourceFile = f})
    let mil = generatedMil state
    logMil mil
    optMil <- optimize (OptimizerEnvironment {optLevel = milcOptLevel options}) mil
    amCode <- generateAMCode optMil
    liftIO $ print amCode
    let codegenEnv = CodeGeneratorEnvironment {codegenSource=s, codegenSourceFile=f, codegenOutDir = milcOutDir options}
    writeEncodeTargetCode codegenEnv amCode
    logMsgLn "\nCOMPILATION SUCCEEDED!\n"

-- run the different stages of the compiler (currently only lexer)
compile :: String -> SourceAction -> MilcOptions -> CompilerMonadT () IO
compile f a options = (doCompilation f a options) `catchCompError` handleCompError where
    handleCompError e = do
        let msg = concat $ ["\nCOMPILATION ERROR", atLocation, ":\n", e, "\n"]
        logMsgLn msg
        liftIO $ hPutStrLn stderr msg
    atLocation = case f of
        "" -> ""
        _  -> " in " ++ f

-- compile a file (argument is path to the file)
compileFile :: MilcOptions -> String -> CompilerMonadT () IO
compileFile options f = compile f (sourceAction $ readFile f) options

-- compile multiple files, merging their compilation output together
compileFiles :: MilcOptions -> [String] -> CompilerMonadT () IO
compileFiles options = mapM_ $ compileFile options

-- invoke action based on parsed command line options
runMain :: MilcOptions -> IO ()
runMain options = if milcHelp options
    then putStrLn helpMessage   -- print help message
    else do                     -- compile all source files
        let comp = if milcStdIn options
            then compile "" (sourceAction getContents) options  -- compile standard input
            else compileFiles options (milcInFiles options)     -- compile source files
        (_, l) <- runCompilerT comp
        when (milcLogFile options /= "") $ writeFile (milcLogFile options) (concat l)

main :: IO ()
main = do
    args <- getArgs
    let options = applyOptionTransforms optionTransforms args defaultMilcOptions
    runMain options
