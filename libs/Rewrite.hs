{-# LANGUAGE RecordWildCards #-}

module Rewrite (Pattern, RwRule) where

import Base
import Control.Monad
import Control.Monad.ST
import qualified Data.HashTable.Class as H
import Egraph
import Utils (findMaybeM)

data Pattern
  = POp Op [Pattern]
  | PVar Symbol
  | PCons Int
  deriving (Eq, Ord, Show)

data RwRule = RwRule
  { lhs :: Pattern,
    rhs :: Pattern
  }
  deriving (Eq, Ord, Show)

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
