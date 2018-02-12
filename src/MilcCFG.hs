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

{- this module implements a Control Flow Graph (CFG) for use with MIL -}

module MilcCFG where

import CompilerEnvironment
import MilcUtils
import MIL

import Data.HashMap.Strict as HashMap
import Data.List
import Control.Monad


-- data type for representing incoming CFG edges
data IncomingEdge   = IncomingJump BlockId
                    | IncomingBranch BlockId
                    | IncomingFallthrough BlockId
                    | Start
                    deriving (Eq)

instance Show IncomingEdge where
    show (IncomingJump bbid) = "Jump " ++ show bbid
    show (IncomingBranch bbid) = "Branch " ++ show bbid
    show (IncomingFallthrough bbid) = "Fallthrough " ++ show bbid
    show Start = "Start"

-- data type for representing outgoing CFG edges
data OutgoingEdge   = OutgoingJump BlockId
                    | OutgoingBranch BlockId
                    | OutgoingFallthrough BlockId
                    | Exit
                    deriving (Eq)

instance Show OutgoingEdge where
    show (OutgoingJump bbid) = "Jump " ++ show bbid
    show (OutgoingBranch bbid) = "Branch " ++ show bbid
    show (OutgoingFallthrough bbid) = "Fallthrough " ++ show bbid
    show Exit = "Exit"

-- data type for the CFG itself
--
-- A CFG is nothing more than a map of basic block IDs to incodming  and
-- outgoing edges
type CFG = HashMap BlockId ([IncomingEdge], [OutgoingEdge])

-- helper for showing a CFG as a string
showCFG :: CFG -> String
showCFG = intercalate "\n" . Prelude.map showNode . toList where
    showNode (bbid, (ins, outs)) = concat [ concat2WithPadding 5 (show bbid) ("in  " ++ show ins), "\n"
                                          , concat2WithPadding 5 "" ("out " ++ show outs)]

-- get the sets of incoming and outgoing edges from a basic block ID
getEdges :: BlockId -> CFG -> Maybe ([IncomingEdge], [OutgoingEdge])
getEdges bbid = HashMap.lookup bbid

-- get the set of incoming edges from a basic block ID
getIncomingEdges :: BlockId -> CFG -> Maybe [IncomingEdge]
getIncomingEdges bbid = liftM fst .getEdges bbid

-- get the set of outgoing edges from a basic block ID
getOutgoingEdges :: BlockId -> CFG -> Maybe [OutgoingEdge]
getOutgoingEdges bbid = liftM snd . getEdges bbid

-- add an incoming edge to a basic block ID in the CFG
addIncomingEdge :: BlockId -> IncomingEdge -> CFG -> CFG
addIncomingEdge bbid edge cfg = HashMap.insert bbid (edge:ins, outs) cfg where
    (ins, outs) = case getEdges bbid cfg of
        Just e -> e
        Nothing -> ([], [])

-- add an outgoing edge to a basic block ID in the CFG
addOutgoingEdge :: BlockId -> OutgoingEdge -> CFG -> CFG
addOutgoingEdge bbid edge cfg = HashMap.insert bbid (ins, edge:outs) cfg where
    (ins, outs) = case getEdges bbid cfg of
        Just e -> e
        Nothing -> ([], [])

-- add a incoming/outgoing Jump edges between two basic blocks
addJump :: BlockId -> BlockId -> CFG -> CFG
addJump source target = addIncomingEdge target (IncomingJump source)
                      . addOutgoingEdge source (OutgoingJump target)

-- add a incoming/outgoing Fallthrough edges between two basic blocks
addFallthrough :: BlockId -> BlockId -> CFG -> CFG
addFallthrough source target = addIncomingEdge target (IncomingFallthrough source)
                             . addOutgoingEdge source (OutgoingFallthrough target)

-- add a incoming/outgoing Branch (plus the accompanying Fallthrough)
-- edges between two basic blocks
addBranch :: BlockId -> BlockId -> BlockId -> CFG -> CFG
addBranch source target fallthrough = addFallthrough source fallthrough
                                    . addIncomingEdge target (IncomingBranch source)
                                    . addOutgoingEdge source (OutgoingBranch target)

-- add edges to a CFG from a list of basic blocks
addEdges :: [BasicBlock] -> CFG -> CFG
addEdges ((BasicBlock bbid _ terminator):bb2:bbs) cfg = addEdges (bb2:bbs) cfg' where
    cfg' = case terminator of
        Jump target -> addJump bbid target cfg
        Branch _ target -> addBranch bbid target (blockId bb2) cfg
        BranchZero _ target -> addBranch bbid target (blockId bb2) cfg
        Fallthrough -> addFallthrough bbid (blockId bb2) cfg
        Return _ -> addOutgoingEdge bbid Exit cfg
addEdges [BasicBlock bbid _ _]  cfg = addOutgoingEdge bbid Exit cfg
addEdges [] cfg = cfg

-- build CFG from MIL code
buildCFG :: Monad m => Mil -> CompilerMonadT CFG m
buildCFG (Mil blocks@(BasicBlock bbid _ _:_)) = return $ addEdges blocks (addIncomingEdge bbid Start empty)
buildCFG (Mil []) = return empty

-- log a CFG as a block
logCFG :: Monad m => CFG -> CompilerMonadT () m
logCFG = logBlock . showCFG

-- return sets of incoming and outgoing edges of a basic block
cfgEdgesOf :: Monad m => BasicBlock -> CFG -> CompilerMonadT ([IncomingEdge], [OutgoingEdge]) m
cfgEdgesOf (BasicBlock bbid _ _) cfg = case getEdges bbid cfg of
    Just e -> return e
    Nothing -> logError ("Basic block " ++ show bbid ++ " is not in the CFG")

-- return list of incoming edges of a basic block
incomingEdgesOf :: Monad m => BasicBlock -> CFG -> CompilerMonadT [IncomingEdge] m
incomingEdgesOf bb = liftM fst . cfgEdgesOf bb

-- return list of outgoing edges of a basic block
outgoingEdgesOf :: Monad m => BasicBlock -> CFG -> CompilerMonadT [OutgoingEdge] m
outgoingEdgesOf bb = liftM snd . cfgEdgesOf bb

-- return true if two basic blocks can be safely merged into one basic block
--
-- Two basic blocks can be merged together iff the first block has exactly one
-- outgoing edge that goes to the second block and the second block has exactly
-- one incoming edge that comes from the first clock
--
--      +--------------+       +--------------+
--      | BasicBlock_1 |------>| BasicBlock_2 |
--      +--------------+       +--------------+
--
canMergeSafely :: Monad m => BasicBlock -> BasicBlock -> CFG -> CompilerMonadT Bool m
canMergeSafely bb1 bb2 cfg = do
    out1 <- outgoingEdgesOf bb1 cfg
    in2 <- incomingEdgesOf bb2 cfg
    return $ isSafe out1 in2
    where
        isSafe [OutgoingFallthrough target] [IncomingFallthrough source] = target == source
        isSafe _ _ = False
