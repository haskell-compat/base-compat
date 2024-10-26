{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Either.Compat"
-- from a globally unique namespace.
module Data.Either.Compat.Repl.Batteries (
  module Data.Either.Compat
) where
import "this" Data.Either.Compat
