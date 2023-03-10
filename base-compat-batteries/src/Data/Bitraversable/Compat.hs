{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Bitraversable.Compat (
  module Base
) where

#if MIN_VERSION_base(4,10,0)
import "base-compat" Data.Bitraversable.Compat as Base
#else
import "bifunctor-classes-compat" Data.Bitraversable as Base
#endif
