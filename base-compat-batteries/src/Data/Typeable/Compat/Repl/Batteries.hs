{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Typeable.Compat"
-- from a globally unique namespace.
module Data.Typeable.Compat.Repl.Batteries (
  module Data.Typeable.Compat
) where
import "this" Data.Typeable.Compat
