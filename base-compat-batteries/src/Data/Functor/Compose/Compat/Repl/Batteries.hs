{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Functor.Compose.Compat"
-- from a globally unique namespace.
module Data.Functor.Compose.Compat.Repl.Batteries (
  module Data.Functor.Compose.Compat
) where
import "this" Data.Functor.Compose.Compat
