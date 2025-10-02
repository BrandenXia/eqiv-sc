{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad.ST
import Data.HashTable.Class as H
import Egraph
import Rewrite

main :: IO ()
main = do
  eg <- stToIO createEGraph
  let oneNode = VarNode "x"
  let zeroNode = ConsNode 0
  eid1 <- stToIO $ addENode eg oneNode
  eid0 <- stToIO $ addENode eg zeroNode
  let node = OpNode "add" [eid1, eid0]
  let rule = RwRule (POp "add" [PVar "x", PCons 0]) (PVar "x")
  eid <- stToIO $ addENode eg node
  neweid <- stToIO $ rewrite eg rule eid
  case neweid of
    Nothing -> putStrLn "No rewrite applied"
    Just nid -> do
      res <- stToIO $ H.lookup (classes eg) nid
      case res of
        Nothing -> putStrLn "No class found"
        Just nodes -> print nodes
      return ()
  return ()
