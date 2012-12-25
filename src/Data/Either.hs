module Data.Either (
  module Base
, isLeft
, isRight
) where
import "base" Data.Either as Base

import Prelude

-- | Return `True` if the given value is a `Left`-value, `False` otherwise.
isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

-- | Return `True` if the given value is a `Right`-value, `False` otherwise.
isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True
