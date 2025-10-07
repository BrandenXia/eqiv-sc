{-# LANGUAGE RecordWildCards #-}

module Rewrite (Pattern (..), RwRule (..), rewrite) where

import Base
import Control.Monad
import Control.Monad.ST
import qualified Data.HashTable.Class as H
import Egraph
import Utils (findMaybeM)

data Pattern
  = POp Op [Pattern]
  | PVar Symbol
  | PCons Primitive
  deriving (Eq, Ord, Show)

data RwRule = RwRule
  { lhs :: Pattern,
    rhs :: Pattern
  }
  deriving (Eq, Ord, Show)

-- TODO: Replace [(Symbol, EClassId)] with a more efficient map structure

matchPattern :: EGraph s -> Pattern -> EClassId -> ST s (Maybe [(Symbol, EClassId)])
matchPattern eg@(EGraph {..}) pattern cid = case pattern of
  PVar v -> return $ Just [(v, cid)]
  _ -> do
    nodes <- H.lookup classes cid
    case nodes of
      Nothing -> return Nothing
      Just ns ->
        case pattern of
          PCons n ->
            return $
              if any (matchCons n) ns
                then Just []
                else Nothing
            where
              matchCons cons (ConsNode m) = m == cons
              matchCons _ _ = False
          POp op args ->
            let matchOp (OpNode op' args')
                  | op /= op' || length args /= length args' = return Nothing
                  | otherwise = do
                      matches <- zipWithM (matchPattern eg) args args'
                      return $ concat <$> sequence matches
                matchOp _ = return Nothing
             in findMaybeM matchOp ns

instantiatePattern :: EGraph s -> Pattern -> [(Symbol, EClassId)] -> ST s EClassId
instantiatePattern eg pattern subs = case pattern of
  PVar v -> case lookup v subs of
    Just cid -> return cid
    Nothing -> error $ "Unbound variable: " ++ v
  PCons n -> addENode eg (ConsNode n)
  POp op args -> do
    argIds <- mapM (\p -> instantiatePattern eg p subs) args
    addENode eg (OpNode op argIds)

rewrite :: EGraph s -> RwRule -> EClassId -> ST s (Maybe EClassId)
rewrite eg (RwRule lhs rhs) eid = do
  match <- matchPattern eg lhs eid
  case match of
    Nothing -> return Nothing
    Just subs -> do
      rhsId <- instantiatePattern eg rhs subs
      return $ Just rhsId
