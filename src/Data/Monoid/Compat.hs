module Data.Monoid.Compat (
  module Base
, (<>)
) where
import Data.Monoid as Base

#if !MIN_VERSION_base(4,5,0)
-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif
