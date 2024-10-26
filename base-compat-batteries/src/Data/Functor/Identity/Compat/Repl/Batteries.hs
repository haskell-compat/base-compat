{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Functor.Identity.Compat"
-- from a globally unique namespace.
module Data.Functor.Identity.Compat.Repl.Batteries (
  module Data.Functor.Identity.Compat
) where
import "this" Data.Functor.Identity.Compat
