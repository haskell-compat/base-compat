{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Bifoldable.Compat"
-- from a globally unique namespace.
module Data.Bifoldable.Compat.Repl.Batteries (
  module Data.Bifoldable.Compat
) where
import "this" Data.Bifoldable.Compat
