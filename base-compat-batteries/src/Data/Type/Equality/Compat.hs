{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Data.Type.Equality.Compat (
  -- * The equality types
  (:~:)(..),
  type (~~),
  (:~~:)(..),

  -- * Working with equality
  sym, trans, castWith, gcastWith,
  apply,
  inner,
  outer,

  -- * Inferring equality from other types
  TestEquality(..),

  -- * Boolean type-level equality
  type (==),
) where

import "base" Data.Type.Equality

#if !MIN_VERSION_base(4,10,0)
import "type-equality" Data.Type.Equality.Hetero
#endif
