{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Functor.Compat"
-- from a globally unique namespace.
module Data.Functor.Compat.Repl.Batteries (
  module Data.Functor.Compat
) where
import "this" Data.Functor.Compat
