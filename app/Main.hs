{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Base
import Control.Monad.ST
import Egraph
import Rewrite
import Search

main :: IO ()
main = do
  eg <- stToIO createEGraph
  let oneNode = VarNode "x"
      zeroNode = ConsNode (PrimInt 0)
  xId <- stToIO $ addENode eg oneNode -- x
  zeroId <- stToIO $ addENode eg zeroNode -- 0
  let node = OpNode "+" [xId, zeroId]
  eid1 <- stToIO $ addENode eg node -- add(x, 0)
  let node2 = OpNode "+" [zeroId, eid1]
  eid2 <- stToIO $ addENode eg node2 -- add(0, add(x, 0))
  --
  let rule1 = RwRule (POp "+" [PVar "x", PCons (PrimInt 0)]) (PVar "x")
      rule2 = RwRule (POp "+" [PVar "x", PVar "y"]) (POp "+" [PVar "y", PVar "x"])
  _ <- stToIO $ saturate eg [rule1, rule2] eid2

  str <- stToIO $ visualizeEGraph eg
  putStrLn str
  return ()
