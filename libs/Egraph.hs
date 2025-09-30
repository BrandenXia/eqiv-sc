{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Egraph (EClassId, ENode (..), EGraph (..), createEGraph, addENode, unionENodes) where

import Base
import Control.Monad (when)
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as HashTable
import Data.Hashable (Hashable)
import qualified Data.Vector.Unboxed.Mutable as MVU
import GHC.Generics (Generic)

type EClassId = Int

data UnionFind s = UnionFind
  { parents :: MVU.STVector s EClassId,
    ranks :: MVU.STVector s Int
  }

createUF :: ST s (UnionFind s)
createUF = do
  p <- MVU.new 0
  r <- MVU.new 0
  return $ UnionFind p r

uf_find :: UnionFind s -> EClassId -> ST s EClassId
uf_find uf@(UnionFind p _) x = do
  px <- MVU.read p x
  if px == x
    then return x
    else do
      root <- uf_find uf px
      MVU.write p x root
      return root

uf_union :: UnionFind s -> EClassId -> EClassId -> ST s EClassId
uf_union uf@(UnionFind {..}) x y = do
  px <- uf_find uf x
  py <- uf_find uf y
  if (px == py)
    then return px
    else do
      rx <- MVU.read ranks px
      ry <- MVU.read ranks py
      if rx < ry
        then do
          MVU.write parents px py
          MVU.write ranks py (ry + rx)
          return py
        else do
          MVU.write parents py px
          MVU.write ranks px (rx + ry)
          return px

-- TODO: Replace this with a dynamic vector that resizes to 1.5x its size when full with capacity and length tracking
uf_add :: UnionFind s -> ST s EClassId
uf_add UnionFind {..} = do
  let len = MVU.length parents
  let newId = len
  p' <- MVU.grow parents 1
  r' <- MVU.grow ranks 1
  MVU.write p' newId newId
  MVU.write r' newId 1
  return newId

data ENode
  = OpNode Op [EClassId]
  | VarNode Symbol
  | ConsNode Int
  deriving (Eq, Ord, Show, Generic)

instance Hashable ENode

data EGraph s = EGraph
  { classes :: HashTable.HashTable s EClassId [ENode],
    memo :: HashTable.HashTable s ENode EClassId,
    uf :: UnionFind s
  }

createEGraph :: ST s (EGraph s)
createEGraph = do
  cls <- HashTable.new
  m <- HashTable.new
  u <- createUF
  return $ EGraph cls m u

addENode :: EGraph s -> ENode -> ST s EClassId
addENode EGraph {..} node =
  case node of
    OpNode op args -> do
      args' <- mapM (uf_find uf) args
      let node' = OpNode op args'
      cid <- HashTable.lookup memo node'
      case cid of
        Just c -> return c
        Nothing -> do
          newId <- uf_add uf
          HashTable.insert memo node' newId
          HashTable.insert classes newId [node']
          return newId
    _ -> do
      cid <- HashTable.lookup memo node
      case cid of
        Just c -> return c
        Nothing -> do
          newId <- uf_add uf
          HashTable.insert memo node newId
          HashTable.insert classes newId [node]
          return newId

unionENodes :: EGraph s -> EClassId -> EClassId -> ST s ()
unionENodes EGraph {..} id1 id2 = do
  root1 <- uf_find uf id1
  root2 <- uf_find uf id2
  when (root1 /= root2) $ do
    root <- uf_union uf root1 root2
    nodes1 <- HashTable.lookup classes root1
    nodes2 <- HashTable.lookup classes root2
    let mergedNodes = maybe [] id nodes1 ++ maybe [] id nodes2
    mapM_ (\node -> HashTable.insert memo node root) mergedNodes
    HashTable.insert classes root mergedNodes
    HashTable.delete classes (if root == root1 then root2 else root1)
