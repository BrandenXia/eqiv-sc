{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Blammo.Logging.Simple (MonadLoggerIO (askLoggerIO), runSimpleLoggingT)
import Cli
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Repl
import Runtime

main :: IO ()
main = do
  Cli {..} <- runCli
  logFunc <-
    if verbose opts
      then runSimpleLoggingT askLoggerIO
      else runNoLoggingT askLoggerIO
  env <- createEnv opts logFunc
  case cmd of
    Repl -> runAppM env repl
    RunFile _ -> return ()
