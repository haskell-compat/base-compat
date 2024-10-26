{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
module Numeric.Natural.Compat (
  module Base
, minusNaturalMaybe
) where

import Numeric.Natural as Base

#if !(MIN_VERSION_base(4,18,0))
import GHC.Natural (minusNaturalMaybe)
#endif
