{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Traversable.Compat"
-- from a globally unique namespace.
module Data.Traversable.Compat.Repl (
  module Data.Traversable.Compat
) where
import "this" Data.Traversable.Compat
