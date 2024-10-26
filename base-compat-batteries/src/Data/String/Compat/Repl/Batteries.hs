{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.String.Compat"
-- from a globally unique namespace.
module Data.String.Compat.Repl.Batteries (
  module Data.String.Compat
) where
import "this" Data.String.Compat
