{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad.ST
import Data.HashTable.Class as H
import Egraph
import Rewrite
import Search

main :: IO ()
main = do
  eg <- stToIO createEGraph
  let oneNode = VarNode "x"
  let zeroNode = ConsNode 0
  eid1 <- stToIO $ addENode eg oneNode
  eid0 <- stToIO $ addENode eg zeroNode
  let node = OpNode "add" [eid1, eid0]
  let rule1 = RwRule (POp "add" [PVar "x", PCons 0]) (PVar "x")
  let rule2 = RwRule (POp "add" [PCons 0, PVar "x"]) (PVar "x")
  let rule3 = RwRule (POp "add" [PVar "x", PVar "y"]) (POp "add" [PVar "y", PVar "x"])
  eid <- stToIO $ addENode eg node
  _ <- stToIO $ saturate eg [rule1, rule2, rule3] eid
  res <- stToIO $ H.lookup (classes eg) eid
  case res of
    Nothing -> putStrLn "No class found"
    Just nodes -> print nodes
  return ()
