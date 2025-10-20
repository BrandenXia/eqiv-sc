{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Repl (repl) where

import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Parser
import Runtime
import System.Console.Haskeline

settings :: (MonadIO m) => Settings m
settings = defaultSettings

loop :: InputT App ()
loop = forever $ do
  maybeInput <- getInputLine "> "
  when (isJust maybeInput) $ do
    let input = fromJust maybeInput
    case parseAst "repl" input of
      Left err -> outputStrLn err
      Right asts -> lift $ do
        $logDebug $ "Parsed ASTs from input:" <> T.pack (show asts)
        runAsts asts

repl :: App ()
repl = runInputT settings $ withInterrupt $ handleInterrupt (pure ()) loop
