{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.IORef.Compat"
-- from a globally unique namespace.
module Data.IORef.Compat.Repl.Batteries (
  module Data.IORef.Compat
) where
import "this" Data.IORef.Compat
