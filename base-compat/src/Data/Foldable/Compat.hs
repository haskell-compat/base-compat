{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Foldable.Compat (
  module Base
#if !(MIN_VERSION_base(4,10,0))
, maximumBy
, minimumBy
#endif
) where

#if MIN_VERSION_base(4,10,0)
import Data.Foldable as Base
#else
import Data.Foldable as Base hiding (maximumBy, minimumBy)
import Prelude (Ordering(..))
#endif

#if !(MIN_VERSION_base(4,10,0))
-- | The largest element of a non-empty structure with respect to the
-- given comparison function.
maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = foldl1 max'
  where max' x y = case cmp x y of
                        GT -> x
                        _  -> y

-- | The least element of a non-empty structure with respect to the
-- given comparison function.
minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = foldl1 min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x
#endif
