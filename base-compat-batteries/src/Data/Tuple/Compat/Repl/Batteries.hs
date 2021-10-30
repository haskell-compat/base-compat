{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Tuple.Compat"
-- from a globally unique namespace.
module Data.Tuple.Compat.Repl.Batteries (
  module Data.Tuple.Compat
) where
import "this" Data.Tuple.Compat
