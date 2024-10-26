{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Bits.Compat"
-- from a globally unique namespace.
module Data.Bits.Compat.Repl (
  module Data.Bits.Compat
) where
import "this" Data.Bits.Compat
