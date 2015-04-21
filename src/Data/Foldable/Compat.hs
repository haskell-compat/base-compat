{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Foldable.Compat (
  module Base
#if !(MIN_VERSION_base(4,8,0))
, length
, null
#endif
) where

import Data.Foldable as Base

#if !(MIN_VERSION_base(4,8,0))
import Prelude (Bool(..), Int, (+))

-- | Test whether the structure is empty. The default implementation is
-- optimized for structures that are similar to cons-lists, because there
-- is no general way to do better.
null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True

-- | Returns the size/length of a finite structure as an 'Int'.  The
-- default implementation is optimized for structures that are similar to
-- cons-lists, because there is no general way to do better.
length :: Foldable t => t a -> Int
length = foldl' (\c _ -> c+1) 0
#endif
