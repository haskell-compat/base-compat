{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Functor.Sum.Compat"
-- from a globally unique namespace.
module Data.Functor.Sum.Compat.Repl.Batteries (
  module Data.Functor.Sum.Compat
) where
import "this" Data.Functor.Sum.Compat
