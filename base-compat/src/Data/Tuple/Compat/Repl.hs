{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Tuple.Compat"
-- from a globally unique namespace.
module Data.Tuple.Compat.Repl (
  module Data.Tuple.Compat
) where
import "this" Data.Tuple.Compat
