{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.STRef.Compat"
-- from a globally unique namespace.
module Data.STRef.Compat.Repl.Batteries (
  module Data.STRef.Compat
) where
import "this" Data.STRef.Compat
