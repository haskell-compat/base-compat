{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Traversable.Compat"
-- from a globally unique namespace.
module Data.Traversable.Compat.Repl.Batteries (
  module Data.Traversable.Compat
) where
import "this" Data.Traversable.Compat
