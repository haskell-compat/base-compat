{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Numeric.Natural.Compat (
#if MIN_VERSION_base(4,8,0)
  module Base
, minusNaturalMaybe
#endif
) where

#if MIN_VERSION_base(4,8,0)
import Numeric.Natural as Base

# if !(MIN_VERSION_base(4,18,0))
import GHC.Natural (minusNaturalMaybe)
# endif
#endif
