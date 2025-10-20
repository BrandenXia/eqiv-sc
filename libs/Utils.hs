module Utils (findMaybeM, maximumOn) where

import Control.Monad (foldM)

findMaybeM :: (Foldable t, Monad m) => (a -> m (Maybe b)) -> t a -> m (Maybe b)
findMaybeM f = foldM step Nothing
  where
    step Nothing x = f x
    step v _ = return v

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
maximumOn f xs = fst <$> foldr step Nothing xs
  where
    step x Nothing = Just (x, f x)
    step x (Just prev@(_, fy)) =
      let fx = f x
       in Just $ if fx > fy then (x, fx) else prev
