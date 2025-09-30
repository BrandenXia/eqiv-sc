module Main (main) where

import qualified Data.Vector.Mutable as MV

main :: IO ()
main = do
  vec <- MV.new 10 :: IO (MV.IOVector Int)
  putStrLn $ show (MV.length vec)
  MV.write vec 0 100
  MV.write vec 1 200
  putStrLn $ show (MV.length vec)
