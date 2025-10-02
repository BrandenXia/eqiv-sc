module Utils (findMaybeM) where

import Control.Monad (foldM)

findMaybeM :: (Foldable t, Monad m) => (a -> m (Maybe b)) -> t a -> m (Maybe b)
findMaybeM f xs = foldM step Nothing xs
  where
    step (Just v) _ = return (Just v)
    step Nothing x = f x
