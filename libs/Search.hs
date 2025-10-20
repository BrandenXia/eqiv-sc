{-# LANGUAGE RecordWildCards #-}

module Search (saturate, findBestRepr) where

import Control.Monad.ST
import qualified Data.HashTable.Class as H
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set
import Egraph
import Parser
import Simplify
import Utils (maximumOn)

saturate :: EGraph s -> [RwRule] -> EClassId -> ST s ()
saturate eg rules eid = do
  bfs [eid] Set.empty
  where
    bfs [] _ = return ()
    bfs (x : xs) visited = do
      root <- findEClass eg x
      rws <- mapM (flip (rewrite eg) root) rules
      comp <- compute eg root
      let rws' = catMaybes (comp : rws)
      mapM_ (unionENodes eg root) rws'
      let queue = xs ++ filter (not . flip Set.member visited) rws'
      bfs queue (Set.insert x visited)
      return ()

-- find the best expression representation for an e-class, eid should exist in e-graph
findBestRepr :: EGraph s -> EClassId -> ST s Expr
findBestRepr eg@(EGraph {..}) eid = do
  root <- findEClass eg eid
  nodes <- fromJust <$> H.lookup classes root
  case fromJust $ maximumOn scoreEnode nodes of
    OpNode op args -> do
      argExprs <- mapM (findBestRepr eg) args
      return $ OpExpr op argExprs
    VarNode v -> return $ VarExpr v
    ConsNode n -> return $ ConsExpr n
  where
    scoreEnode :: ENode -> Int
    scoreEnode (ConsNode _) = 3
    scoreEnode (VarNode _) = 2
    scoreEnode (OpNode op _) = case op of
      "+" -> 1
      "*" -> 1
      "'" -> -1
      _ -> 0
