{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Foldable1.Compat (
  module Base
) where

#if MIN_VERSION_base(4,18,0)
import "base-compat" Data.Foldable1.Compat as Base
#else
import "foldable1-classes-compat" Data.Foldable1 as Base
#endif
