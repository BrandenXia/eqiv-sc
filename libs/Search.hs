{-# LANGUAGE RecordWildCards #-}

module Search (saturate) where

import Control.Monad.ST
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Egraph
import Rewrite

saturate :: EGraph s -> [RwRule] -> EClassId -> ST s ()
saturate eg rules eid = do
  bfs [eid] Set.empty
  return ()
  where
    bfs [] _ = return ()
    bfs (x : xs) visited = do
      rws' <- mapM (flip (rewrite eg) x) rules
      let rws = catMaybes rws'
      mapM_ (unionENodes eg x) rws
      bfs (filter (flip Set.member visited) xs ++ rws) (Set.insert x visited)
      return ()
