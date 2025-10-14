{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (forever)
import Control.Monad.ST
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Text as T
import Egraph
import Parser
import Search
import Simplify
import System.IO
import Text.Megaparsec (runParser)

runEgraph :: EGraph s -> STRef s [RwRule] -> Ast -> ST s ()
runEgraph _ rulesRef (ARewrite lhs rhs) = do
  let rule = expr2RwRule lhs rhs
  rules <- readSTRef rulesRef
  writeSTRef rulesRef $ rule : rules
runEgraph eg rulesRef (AExpr expr) = do
  eid <- expr2ENode eg expr
  rules <- readSTRef rulesRef
  saturate eg rules eid

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  eg <- stToIO createEGraph
  rulesRef <- stToIO $ newSTRef []
  forever $ do
    putStr "> "
    input <- getLine
    case runParser pAst "repl" $ T.pack input of
      Left err -> putStrLn $ show err
      Right ast -> do
        stToIO $ runEgraph eg rulesRef ast
        str <- stToIO $ visualizeEGraph eg
        putStrLn str
