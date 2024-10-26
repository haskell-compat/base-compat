{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.List.Compat"
-- from a globally unique namespace.
module Data.List.Compat.Repl (
  module Data.List.Compat
) where
import "this" Data.List.Compat
