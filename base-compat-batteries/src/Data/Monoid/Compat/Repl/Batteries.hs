{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Monoid.Compat"
-- from a globally unique namespace.
module Data.Monoid.Compat.Repl.Batteries (
  module Data.Monoid.Compat
) where
import "this" Data.Monoid.Compat
