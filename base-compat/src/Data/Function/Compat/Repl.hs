{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Function.Compat"
-- from a globally unique namespace.
module Data.Function.Compat.Repl (
  module Data.Function.Compat
) where
import "this" Data.Function.Compat
