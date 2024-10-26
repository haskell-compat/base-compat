{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Type.Equality.Compat"
-- from a globally unique namespace.
module Data.Type.Equality.Compat.Repl.Batteries (
  module Data.Type.Equality.Compat
) where
import "this" Data.Type.Equality.Compat
