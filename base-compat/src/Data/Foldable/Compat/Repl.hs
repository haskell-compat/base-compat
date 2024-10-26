{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Foldable.Compat"
-- from a globally unique namespace.
module Data.Foldable.Compat.Repl (
  module Data.Foldable.Compat
) where
import "this" Data.Foldable.Compat
