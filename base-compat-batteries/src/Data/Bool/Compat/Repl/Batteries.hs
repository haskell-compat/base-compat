{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Bool.Compat"
-- from a globally unique namespace.
module Data.Bool.Compat.Repl.Batteries (
  module Data.Bool.Compat
) where
import "this" Data.Bool.Compat
