{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Runtime (createEnv, runAst, runAsts, Env (..), App (..), runAppM) where

import Cli
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.ST
import Data.IORef
import qualified Data.Text as T
import Egraph
import GHC.IORef (atomicModifyIORef'_)
import Parser
import Search
import Simplify hiding (lhs, rhs)

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

data Env = Env
  { envOpts :: Opts,
    envEgraph :: EGraph RealWorld,
    envRules :: IORef [RwRule],
    envLogger :: LogFunc
  }

newtype App a = App {unApp :: ReaderT Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)
  deriving newtype (MonadThrow, MonadCatch, MonadMask)

runAppM :: Env -> App a -> IO a
runAppM env (unApp -> app) = runReaderT app env

instance MonadLogger App where
  monadLoggerLog loc src level (toLogStr -> msg) = do
    logger <- asks envLogger
    liftIO $ logger loc src level msg

createEnv :: Opts -> LogFunc -> IO Env
createEnv opts logger = do
  eg <- stToIO createEGraph
  rules <- newIORef []
  return $
    Env
      { envOpts = opts,
        envEgraph = eg,
        envRules = rules,
        envLogger = logger
      }

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
expr2RwRule (expr2Pattern -> lhs) (expr2Pattern -> rhs) = RwRule lhs rhs

addRule :: RwRule -> App ()
addRule newRule = do
  rulesRef' <- asks envRules
  void . liftIO $ atomicModifyIORef'_ rulesRef' $ (:) newRule
  $logDebug $ "Added new rewrite rule: " <> T.pack (show newRule)

getRules :: App [RwRule]
getRules = do
  rulesRef' <- asks envRules
  rules <- liftIO $ readIORef rulesRef'
  $logDebug $ "Current rewrite rules: " <> T.pack (show rules)
  return rules

addExpr :: Expr -> App EClassId
addExpr = runEGraphOp . flip expr2ENode

runSaturation :: [RwRule] -> EClassId -> App ()
runSaturation rules eid = runEGraphOp (\eg -> saturate eg rules eid)

runEGraphOp :: (EGraph RealWorld -> ST RealWorld a) -> App a
runEGraphOp stAction = do
  theEGraph <- asks envEgraph
  liftIO . stToIO $ stAction theEGraph

runAst :: Ast -> App ()
runAst (ARewrite lhs rhs) = do
  addRule (expr2RwRule lhs rhs)
runAst (AExpr expr) = do
  eid <- addExpr expr
  rules <- getRules
  let showEGraph = fmap T.pack . liftIO . stToIO . visualizeEGraph
  $logDebug =<< showEGraph =<< asks envEgraph
  runSaturation rules eid

runAsts :: [Ast] -> App ()
runAsts = mapM_ runAst
