{-# LANGUAGE RecordWildCards #-}

module Utils.MVec (MVec, length, new, read, write, pushBack) where

import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as MVU
import Prelude hiding (length, read)

data MVec' s a = MVec'
  { d :: (MVU.MVector s a),
    size :: Int,
    capacity :: Int
  }

newtype MVec s a = MVec (STRef s (MVec' s a))

length :: MVec s a -> ST s Int
length (MVec ref) = do
  MVec' {..} <- readSTRef ref
  return size

new :: (MVU.Unbox a) => Int -> ST s (MVec s a)
new cap = do
  vec <- V.new cap
  ref <- newSTRef (MVec' vec 0 cap)
  return (MVec ref)

read :: (MVU.Unbox a) => MVec s a -> Int -> ST s a
read (MVec ref) idx = do
  MVec' {..} <- readSTRef ref
  if idx < 0 || idx >= size
    then error "Index out of bounds"
    else V.read d idx

write :: (MVU.Unbox a) => MVec s a -> Int -> a -> ST s ()
write (MVec ref) idx val = do
  MVec' {..} <- readSTRef ref
  if idx < 0 || idx >= size
    then error "Index out of bounds"
    else V.write d idx val

growFactor :: Double
growFactor = 1.5

pushBack :: (MVU.Unbox a) => MVec s a -> a -> ST s ()
pushBack (MVec ref) val = do
  MVec' {..} <- readSTRef ref
  if size < capacity
    then do
      V.write d size val
      writeSTRef ref (MVec' d (size + 1) capacity)
    else do
      let newCap = max 1 (floor $ fromIntegral capacity * growFactor + 1)
      newD <- V.new newCap
      V.copy (V.slice 0 size newD) (V.slice 0 size d)
      V.write newD size val
      writeSTRef ref (MVec' newD (size + 1) newCap)
