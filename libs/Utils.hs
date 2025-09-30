module Utils (findMaybeM) where

findMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findMaybeM _ [] = return Nothing
findMaybeM f (x : xs) = do
  res <- f x
  case res of
    Just v -> return $ Just v
    Nothing -> findMaybeM f xs
