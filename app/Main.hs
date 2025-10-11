{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Base
import Control.Monad.ST
import Egraph
import Search
import Simplify

main :: IO ()
main = do
  eg <- stToIO createEGraph
  let oneNode = ConsNode (PrimNum 1)
      twoNode = ConsNode (PrimNum 2)
  eid1 <- stToIO $ addENode eg oneNode
  eid2 <- stToIO $ addENode eg twoNode
  eid3 <- stToIO $ addENode eg (OpNode "+" [eid1, eid2])

  let rule1 = RwRule (POp "+" [PVar "x", PCons (PrimNum 0)]) (PVar "x")
      rule2 = RwRule (POp "+" [PVar "x", PVar "y"]) (POp "+" [PVar "y", PVar "x"])
  _ <- stToIO $ saturate eg [rule1, rule2] eid3

  str <- stToIO $ visualizeEGraph eg
  putStrLn str
  return ()
