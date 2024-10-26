{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Complex.Compat"
-- from a globally unique namespace.
module Data.Complex.Compat.Repl.Batteries (
  module Data.Complex.Compat
) where
import "this" Data.Complex.Compat
