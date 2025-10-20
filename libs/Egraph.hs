{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Egraph
  ( EClassId,
    ENode (..),
    EGraph (..),
    createEGraph,
    addENode,
    unionENodes,
    inEGraph,
    addEquivNode,
    findEClass,
    visualizeEGraph,
  )
where

import Base
import Control.Monad.ST
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as BSTH
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import qualified Utils.MVec as MVec

type EClassId = Int

type HashTable s k v = BSTH.HashTable s k v

data UnionFind s = UnionFind
  { parents :: MVec.MVec s EClassId,
    ranks :: MVec.MVec s Int
  }

createUF :: ST s (UnionFind s)
createUF = do
  p <- MVec.new 64
  r <- MVec.new 64
  return $ UnionFind p r

ufFind :: UnionFind s -> EClassId -> ST s EClassId
ufFind uf@(UnionFind p _) x = do
  px <- MVec.read p x
  if px == x
    then return x
    else do
      root <- ufFind uf px
      MVec.write p x root
      return root

ufFindSafe :: UnionFind s -> EClassId -> ST s EClassId
ufFindSafe uf@(UnionFind p _) x = do
  len <- MVec.length p
  if x < 0 || x >= len
    then error "uf_find_safe: EClassId out of bounds"
    else ufFind uf x

ufUnion :: UnionFind s -> EClassId -> EClassId -> ST s EClassId
ufUnion uf@(UnionFind {..}) x y = do
  px <- ufFind uf x
  py <- ufFind uf y
  if px == py
    then return px
    else do
      rx <- MVec.read ranks px
      ry <- MVec.read ranks py
      if rx < ry
        then do
          MVec.write parents px py
          MVec.write ranks py (ry + rx)
          return py
        else do
          MVec.write parents py px
          MVec.write ranks px (rx + ry)
          return px

ufAdd :: UnionFind s -> ST s EClassId
ufAdd UnionFind {..} = do
  newId <- MVec.length parents
  MVec.pushBack parents newId
  MVec.pushBack ranks 1
  return newId

data ENode
  = OpNode Op [EClassId]
  | VarNode Symbol
  | ConsNode Primitive
  deriving (Eq, Ord, Show, Generic)

instance Hashable ENode

data EGraph s = EGraph
  { classes :: HashTable s EClassId (Set.Set ENode),
    memo :: HashTable s ENode EClassId,
    uf :: UnionFind s
  }

createEGraph :: ST s (EGraph s)
createEGraph = do
  cls <- H.new
  m <- H.new
  EGraph cls m <$> createUF

addENode :: EGraph s -> ENode -> ST s EClassId
addENode EGraph {..} (OpNode op args) = do
  args' <- mapM (ufFindSafe uf) args
  let node = OpNode op args'
  cid <- H.lookup memo node
  case cid of
    Just c -> return c
    Nothing -> do
      newId <- ufAdd uf
      H.insert memo node newId
      H.insert classes newId (Set.singleton node)
      return newId
addENode EGraph {..} node = do
  cid <- H.lookup memo node
  case cid of
    Just c -> return c
    Nothing -> do
      newId <- ufAdd uf
      H.insert memo node newId
      H.insert classes newId (Set.singleton node)
      return newId

unionENodes :: EGraph s -> EClassId -> EClassId -> ST s EClassId
unionENodes EGraph {..} id1 id2 = do
  root1 <- ufFind uf id1
  root2 <- ufFind uf id2
  if root1 == root2
    then return root1
    else do
      root <- ufUnion uf root1 root2
      nodes1 <- H.lookup classes root1
      nodes2 <- H.lookup classes root2
      let mergedNodes = fromMaybe Set.empty nodes1 <> fromMaybe Set.empty nodes2
      mapM_ (\node -> H.insert memo node root) mergedNodes
      H.insert classes root mergedNodes
      H.delete classes (if root == root1 then root2 else root1)
      return root

-- Add a new node that is equivalent to an existing EClassId, return eids as (root, new)
addEquivNode :: EGraph s -> ENode -> EClassId -> ST s (EClassId, EClassId)
addEquivNode eg node eid = do
  newEid <- addENode eg node
  root <- unionENodes eg newEid eid
  return (root, newEid)

inEGraph :: EGraph s -> ENode -> ST s (Maybe EClassId)
inEGraph EGraph {..} = H.lookup memo

findEClass :: EGraph s -> EClassId -> ST s EClassId
findEClass EGraph {..} = ufFindSafe uf

visualizeEGraph :: EGraph s -> ST s String
visualizeEGraph EGraph {..} = do
  cnodes <- H.toList classes
  nodesStrs <- mapM showCNode cnodes
  return $ "EGraph {\n  " ++ intercalate "\n  " nodesStrs ++ "\n}"
  where
    showNode (OpNode op args) = do
      argsClass <- mapM (ufFindSafe uf) args
      return $ "OpNode " ++ op ++ " [" ++ intercalate ", " (("C" ++) . show <$> argsClass) ++ "]"
    showNode (VarNode v) = return $ "VarNode " ++ v
    showNode (ConsNode n) = return $ "ConsNode " ++ show n
    showCNode (cid, nodes) = do
      snodes <- mapM showNode (Set.toList nodes)
      return $ "Class" ++ show cid ++ " = {" ++ intercalate ", " snodes ++ "}"
