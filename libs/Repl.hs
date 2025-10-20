module Repl (repl) where

import Control.Monad (forever, when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Maybe (fromJust, isJust)
import Parser
import Runtime
import System.Console.Haskeline

settings :: Settings App
settings = defaultSettings

loop :: InputT App ()
loop = do
  forever $ do
    maybeInput <- getInputLine "> "
    when (isJust maybeInput) $ do
      let input = fromJust maybeInput
      case parseAst "repl" input of
        Left err -> outputStrLn err
        Right asts -> lift $ runAsts asts

repl :: App ()
repl = do
  runInputT settings $ withInterrupt $ handleInterrupt (pure ()) loop
