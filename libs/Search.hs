module Search (saturate) where

import Control.Monad.ST
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Egraph
import Simplify

saturate :: EGraph s -> [RwRule] -> EClassId -> ST s ()
saturate eg rules eid = do
  bfs [eid] Set.empty
  return ()
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
