module Repl (repl) where

import Control.Monad (forever, when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Maybe (fromJust)
import Parser
import Runtime
import System.Console.Haskeline

settings :: Settings App
settings = defaultSettings

loop :: InputT App ()
loop = do
  forever $ do
    maybeInput <- getInputLine "> "
    when (maybeInput /= Nothing) $ do
      let input = fromJust maybeInput
      case parseAst "repl" input of
        Left err -> outputStrLn err
        Right (Just ast) -> do
          lift $ runExpr ast
          return ()
        Right Nothing -> return ()

repl :: App ()
repl = do
  runInputT settings $ withInterrupt $ handleInterrupt (return ()) loop
