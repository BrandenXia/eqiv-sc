{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Cli
import Repl

main :: IO ()
main = do
  cli <- runCli
  case cmd cli of
    Repl -> repl
    RunFile _ -> return ()
