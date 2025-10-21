{-# LANGUAGE RecordWildCards #-}

module Simplify (Pattern (..), RwRule (..), rewrite, compute) where

import Base
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable (Foldable (toList))
import Data.Functor
import qualified Data.HashTable.Class as H
import Data.Maybe (listToMaybe, mapMaybe)
import Egraph
import Utils (findMaybeM)

data Pattern
  = POp Op [Pattern]
  | PVar Symbol
  | PCons Primitive
  deriving (Eq, Ord)

instance Show Pattern where
  show (POp op args) = "(" ++ op ++ " " ++ unwords (map show args) ++ ")"
  show (PVar v) = v
  show (PCons (PrimNum n)) = show (fromRational n :: Double)

data RwRule = RwRule
  { lhs :: Pattern,
    rhs :: Pattern
  }
  deriving (Eq, Ord, Show)

-- TODO: Replace [(Symbol, EClassId)] with a more efficient map structure

matchPattern :: EGraph s -> Pattern -> EClassId -> ST s (Maybe [(Symbol, EClassId)])
matchPattern _ (PVar v) cid = return $ Just [(v, cid)]
matchPattern EGraph {..} (PCons n) cid = runMaybeT $ do
  ns <- MaybeT $ H.lookup classes cid
  guard $ any (matchCons n) ns
  return []
  where
    matchCons cons (ConsNode m) = m == cons
    matchCons _ _ = False
matchPattern eg@(EGraph {..}) (POp op args) cid = runMaybeT $ do
  ns <- MaybeT $ H.lookup classes cid
  let matchOp (OpNode op' args')
        | op /= op' || length args /= length args' = return Nothing
        | otherwise = do
            matches <- zipWithM (matchPattern eg) args args'
            return $ concat <$> sequence matches
      matchOp _ = return Nothing
   in MaybeT $ findMaybeM matchOp ns

instantiatePattern :: EGraph s -> [(Symbol, EClassId)] -> Pattern -> ST s EClassId
instantiatePattern _ subs (PVar v) =
  case lookup v subs of
    Just cid -> return cid
    Nothing -> error $ "Unbound variable: " ++ v
instantiatePattern eg _ (PCons n) = addENode eg (ConsNode n)
instantiatePattern eg subs (POp op args) = do
  argIds <- mapM (instantiatePattern eg subs) args
  addENode eg (OpNode op argIds)

rewrite :: EGraph s -> RwRule -> EClassId -> ST s (Maybe EClassId)
rewrite eg (RwRule lhs rhs) eid = runMaybeT $ do
  match <- MaybeT $ matchPattern eg lhs eid
  lift $ instantiatePattern eg match rhs

builtinOps :: [(Op, Int, [Primitive] -> Maybe Primitive)]
builtinOps =
  [ ("+", 2, add),
    ("*", 2, mul)
  ]
  where
    add [PrimNum x, PrimNum y] = Just $ PrimNum (x + y)
    add _ = Nothing

    mul [PrimNum x, PrimNum y] = Just $ PrimNum (x * y)
    mul _ = Nothing

builtinOpsLookup :: [(Op, (Int, [Primitive] -> Maybe Primitive))]
builtinOpsLookup = builtinOps <&> \(op, arity, f) -> (op, (arity, f))

compute :: EGraph s -> EClassId -> ST s (Maybe EClassId)
compute eg@(EGraph {..}) cid = runMaybeT $ do
  ns <- MaybeT $ H.lookup classes cid
  prim <- MaybeT $ findMaybeM computeNode ns
  nid <- lift $ addENode eg (ConsNode prim)
  _ <- lift $ unionENodes eg cid nid
  return nid
  where
    getConsNode (ConsNode n) = Just n
    getConsNode _ = Nothing
    g1 = listToMaybe . mapMaybe getConsNode . toList
    computeNode (OpNode op args) = case lookup op builtinOpsLookup
      <&> \(len, f) ->
        if length args /= len
          then return Nothing
          else
            let transform = f <=< traverse g1
                evaluatedArgs = sequence <$> mapM (findEClass eg >=> H.lookup classes) args
             in fmap (>>= transform) evaluatedArgs of
      Just res -> res
      Nothing -> return Nothing
    computeNode _ = return Nothing
