{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Bifoldable1.Compat (
  module Base
) where

#if MIN_VERSION_base(4,18,0)
import "base-compat" Data.Bifoldable1.Compat as Base
#else
import "foldable1-classes-compat" Data.Bifoldable1 as Base
#endif
