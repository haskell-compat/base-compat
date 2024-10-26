{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Bitraversable.Compat"
-- from a globally unique namespace.
module Data.Bitraversable.Compat.Repl (
  module Data.Bitraversable.Compat
) where
import "this" Data.Bitraversable.Compat
