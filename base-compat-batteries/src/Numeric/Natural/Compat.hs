{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Numeric.Natural.Compat (
  module Base
, minusNaturalMaybe
) where

#if MIN_VERSION_base(4,8,0)
import "base-compat" Numeric.Natural.Compat as Base
#else
import "nats" Numeric.Natural as Base

import "this" Prelude.Compat

-- | 'Natural' subtraction. Returns 'Nothing's for non-positive results.
--
-- /Since: 4.18.0.0/
minusNaturalMaybe :: Natural -> Natural -> Maybe Natural
minusNaturalMaybe x y
  | x < y     = Nothing
  | otherwise = Just (x - y)
#endif
