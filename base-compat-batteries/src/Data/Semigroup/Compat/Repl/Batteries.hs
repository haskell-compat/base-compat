{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Semigroup.Compat"
-- from a globally unique namespace.
module Data.Semigroup.Compat.Repl.Batteries (
  module Data.Semigroup.Compat
) where
import "this" Data.Semigroup.Compat
