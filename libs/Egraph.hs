{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Egraph (EClassId, ENode (..), EGraph (..), createEGraph, addENode, unionENodes) where

import Base
import Control.Monad (when)
import Control.Monad.ST
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as BSTH
import Data.Hashable (Hashable)
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

uf_find :: UnionFind s -> EClassId -> ST s EClassId
uf_find uf@(UnionFind p _) x = do
  px <- MVec.read p x
  if px == x
    then return x
    else do
      root <- uf_find uf px
      MVec.write p x root
      return root

uf_find_safe :: UnionFind s -> EClassId -> ST s EClassId
uf_find_safe uf@(UnionFind p _) x = do
  len <- MVec.length p
  if x < 0 || x >= len
    then error "uf_find_safe: EClassId out of bounds"
    else uf_find uf x

uf_union :: UnionFind s -> EClassId -> EClassId -> ST s EClassId
uf_union uf@(UnionFind {..}) x y = do
  px <- uf_find uf x
  py <- uf_find uf y
  if (px == py)
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

uf_add :: UnionFind s -> ST s EClassId
uf_add UnionFind {..} = do
  newId <- MVec.length parents
  MVec.pushBack parents newId
  MVec.pushBack ranks 1
  return newId

data ENode
  = OpNode Op [EClassId]
  | VarNode Symbol
  | ConsNode Int
  deriving (Eq, Ord, Show, Generic)

instance Hashable ENode

data EGraph s = EGraph
  { classes :: HashTable s EClassId [ENode],
    memo :: HashTable s ENode EClassId,
    uf :: UnionFind s
  }

createEGraph :: ST s (EGraph s)
createEGraph = do
  cls <- H.new
  m <- H.new
  u <- createUF
  return $ EGraph cls m u

addENode :: EGraph s -> ENode -> ST s EClassId
addENode EGraph {..} node =
  case node of
    OpNode op args -> do
      args' <- mapM (uf_find_safe uf) args
      let node' = OpNode op args'
      cid <- H.lookup memo node'
      case cid of
        Just c -> return c
        Nothing -> do
          newId <- uf_add uf
          H.insert memo node' newId
          H.insert classes newId [node']
          return newId
    _ -> do
      cid <- H.lookup memo node
      case cid of
        Just c -> return c
        Nothing -> do
          newId <- uf_add uf
          H.insert memo node newId
          H.insert classes newId [node]
          return newId

unionENodes :: EGraph s -> EClassId -> EClassId -> ST s ()
unionENodes EGraph {..} id1 id2 = do
  root1 <- uf_find uf id1
  root2 <- uf_find uf id2
  when (root1 /= root2) $ do
    root <- uf_union uf root1 root2
    nodes1 <- H.lookup classes root1
    nodes2 <- H.lookup classes root2
    let mergedNodes = maybe [] id nodes1 ++ maybe [] id nodes2
    mapM_ (\node -> H.insert memo node root) mergedNodes
    H.insert classes root mergedNodes
    H.delete classes (if root == root1 then root2 else root1)
