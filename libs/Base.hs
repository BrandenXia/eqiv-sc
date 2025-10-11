{-# LANGUAGE DeriveGeneric #-}

module Base (Op, Symbol, Primitive (..)) where

import Data.Hashable
import GHC.Generics (Generic)

type Op = String

type Symbol = String

data Primitive
  = PrimNum Rational
  deriving (Eq, Ord, Show, Generic)

instance Hashable Primitive
