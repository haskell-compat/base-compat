{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Version.Compat"
-- from a globally unique namespace.
module Data.Version.Compat.Repl.Batteries (
  module Data.Version.Compat
) where
import "this" Data.Version.Compat
