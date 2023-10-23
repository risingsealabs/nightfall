{-# LANGUAGE NamedFieldPuns #-}
module Nightfall.MASM.Callgraph where

import qualified Data.Graph as Graph
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import qualified Nightfall.MASM.Types as M

findCalls :: M.Proc -> [M.ProcName]
findCalls M.Proc {M._procInstrs} = findCalls' =<< _procInstrs

findCalls' :: M.Instruction -> [M.ProcName]
findCalls' (M.Exec name) = [name]
findCalls' M.If {M.thenBranch, M.elseBranch} =
  findCalls' =<< thenBranch <> elseBranch
findCalls' (M.While body) = findCalls' =<< body
findCalls' _ = []

-- Miden requires procedures to be defined before any execs that reference them.
sortProcs :: Map M.ProcName M.Proc -> [(M.ProcName, M.Proc)]
sortProcs procs = extract . v2node <$> Graph.reverseTopSort callGraph
  where
    (callGraph, v2node, _) = Graph.graphFromEdges $ inject <$> Map.toList procs
    inject (name, proc) = (proc, name, findCalls proc)
    extract (proc, name, _) = (name, proc)
