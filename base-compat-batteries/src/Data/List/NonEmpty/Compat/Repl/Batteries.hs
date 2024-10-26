{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.List.NonEmpty.Compat"
-- from a globally unique namespace.
module Data.List.NonEmpty.Compat.Repl.Batteries (
  module Data.List.NonEmpty.Compat
) where
import "this" Data.List.NonEmpty.Compat
