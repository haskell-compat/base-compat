{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Typeable.Compat"
-- from a globally unique namespace.
module Data.Typeable.Compat.Repl (
  module Data.Typeable.Compat
) where
import "this" Data.Typeable.Compat
