{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Prelude.Compat (
  module Base
#if !(MIN_VERSION_base(4,9,0))
, Semi.Semigroup((Semi.<>))
#endif
) where

import "base-compat" Prelude.Compat as Base

#if !(MIN_VERSION_base(4,9,0))
import "semigroups" Data.Semigroup as Semi
#endif
