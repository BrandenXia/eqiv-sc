module Repl (repl) where

import Control.Monad (forever, when)
import Control.Monad.ST (ST, stToIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.STRef
import qualified Data.Text as T
import Egraph
import Parser
import Search
import Simplify hiding (lhs, rhs)
import System.Console.Haskeline
import Text.Megaparsec

settings :: Settings IO
settings = defaultSettings

runEgraph :: EGraph s -> STRef s [RwRule] -> Ast -> ST s ()
runEgraph _ rulesRef (ARewrite lhs rhs) = do
  let rule = expr2RwRule lhs rhs
  rules <- readSTRef rulesRef
  writeSTRef rulesRef $ rule : rules
runEgraph eg rulesRef (AExpr expr) = do
  eid <- expr2ENode eg expr
  rules <- readSTRef rulesRef
  saturate eg rules eid

loop :: InputT IO ()
loop = do
  let liftIO = lift . stToIO
  eg <- liftIO createEGraph
  rulesRef <- liftIO $ newSTRef []
  forever $ do
    maybeInput <- getInputLine "> "
    when (maybeInput /= Nothing) $ do
      Just input <- return maybeInput
      case runParser pAst "repl" $ T.pack input of
        Left err -> outputStrLn $ show err
        Right ast -> do
          liftIO $ runEgraph eg rulesRef ast
          res <- liftIO $ visualizeEGraph eg
          outputStrLn res

repl :: IO ()
repl = do
  runInputT settings $ withInterrupt $ handleInterrupt (return ()) loop
