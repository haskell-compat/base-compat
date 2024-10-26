{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Ratio.Compat"
-- from a globally unique namespace.
module Data.Ratio.Compat.Repl.Batteries (
  module Data.Ratio.Compat
) where
import "this" Data.Ratio.Compat
