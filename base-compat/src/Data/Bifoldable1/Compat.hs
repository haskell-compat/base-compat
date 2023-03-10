{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Bifoldable1.Compat (
#if MIN_VERSION_base(4,18,0)
  module Base
#endif
) where

#if MIN_VERSION_base(4,18,0)
import Data.Bifoldable1 as Base
#endif
