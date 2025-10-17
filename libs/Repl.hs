module Repl (repl) where

import Control.Monad (forever, when)
import Control.Monad.ST (stToIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Egraph
import Parser
import Runtime
import System.Console.Haskeline

settings :: Settings IO
settings = defaultSettings

loop :: InputT IO ()
loop = do
  let liftIO = lift . stToIO
  env <- liftIO $ createEnv "repl"
  forever $ do
    maybeInput <- getInputLine "> "
    when (maybeInput /= Nothing) $ do
      Just input <- return maybeInput
      case parseAst (source env) input of
        Left err -> outputStrLn err
        Right ast -> do
          liftIO $ runEgraph ast env
          res <- liftIO $ visualizeEGraph (egraph env)
          outputStrLn res

repl :: IO ()
repl = do
  runInputT settings $ withInterrupt $ handleInterrupt (return ()) loop
