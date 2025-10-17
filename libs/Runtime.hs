{-# LANGUAGE RecordWildCards #-}

module Runtime (createEnv, runEgraph, Env (..)) where

import Control.Monad.ST
import Data.STRef
import Egraph
import Parser
import Search
import Simplify hiding (lhs, rhs)

data Env s = Env
  { source :: String,
    egraph :: EGraph s,
    rulesRef :: STRef s [RwRule]
  }

type EnvMonad s a = Env s -> ST s a

createEnv :: String -> ST s (Env s)
createEnv source = do
  eg <- createEGraph
  rules <- newSTRef []
  return $ Env source eg rules

expr2ENode :: EGraph s -> Expr -> ST s EClassId
expr2ENode eg (OpExpr op args) = do
  argIds <- mapM (expr2ENode eg) args
  addENode eg (OpNode op argIds)
expr2ENode eg (VarExpr v) = addENode eg (VarNode v)
expr2ENode eg (ConsExpr n) = addENode eg (ConsNode n)

expr2Pattern :: Expr -> Pattern
expr2Pattern (OpExpr op args) = POp op $ expr2Pattern <$> args
expr2Pattern (VarExpr sym) = PVar sym
expr2Pattern (ConsExpr cons) = PCons cons

expr2RwRule :: Expr -> Expr -> RwRule
expr2RwRule lhs rhs = RwRule (expr2Pattern lhs) (expr2Pattern rhs)

runEgraph :: Ast -> EnvMonad s ()
runEgraph (ARewrite lhs rhs) (Env {..}) = do
  let rule = expr2RwRule lhs rhs
  rules <- readSTRef rulesRef
  writeSTRef rulesRef $ rule : rules
runEgraph (AExpr expr) (Env {..}) = do
  eid <- expr2ENode egraph expr
  rules <- readSTRef rulesRef
  saturate egraph rules eid
