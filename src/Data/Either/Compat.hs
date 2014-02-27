module Data.Either.Compat (
  module Base
, isLeft
, isRight
) where
import "base" Data.Either as Base

#if !MIN_VERSION_base(4,7,0)
import Data.Bool (Bool(..))
-- | Return `True` if the given value is a `Left`-value, `False` otherwise.
--
-- /Since: 4.7.0.0/
isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

-- | Return `True` if the given value is a `Right`-value, `False` otherwise.
--
-- /Since: 4.7.0.0/
isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True
#endif
