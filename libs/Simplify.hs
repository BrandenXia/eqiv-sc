{-# LANGUAGE RecordWildCards #-}

module Simplify (Pattern (..), RwRule (..), rewrite, compute, expr2RwRule) where

import Base
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable (Foldable (toList))
import Data.Functor
import qualified Data.HashTable.Class as H
import Data.Maybe (catMaybes, listToMaybe)
import Egraph
import Parser
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
matchPattern _ (PVar v) cid = return $ Just [(v, cid)]
matchPattern eg@(EGraph {..}) pattern cid = runMaybeT $ do
  ns <- MaybeT $ H.lookup classes cid
  case pattern of
    PCons n -> do
      guard $ any (matchCons n) ns
      return []
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
       in MaybeT $ findMaybeM matchOp ns

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
    g1 = listToMaybe . catMaybes . map getConsNode . toList
    computeNode (OpNode op args) =
      case lookup op builtinOpsLookup
        <&> \(len, f) ->
          if length args /= len
            then pure Nothing
            else
              let transform = join . fmap f . traverse g1
                  evaluatedArgs = sequence <$> mapM (findEClass eg >=> H.lookup classes) args
               in fmap (>>= transform) evaluatedArgs of
        Just res -> res
        Nothing -> return Nothing
    computeNode _ = return Nothing

expr2Pattern :: Expr -> Pattern
expr2Pattern (OpExpr op args) = POp op $ expr2Pattern <$> args
expr2Pattern (VarExpr sym) = PVar sym
expr2Pattern (ConsExpr cons) = PCons cons

expr2RwRule :: Expr -> Expr -> RwRule
expr2RwRule lhs rhs = RwRule (expr2Pattern lhs) (expr2Pattern rhs)
