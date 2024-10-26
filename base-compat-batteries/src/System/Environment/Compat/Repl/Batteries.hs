{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "System.Environment.Compat"
-- from a globally unique namespace.
module System.Environment.Compat.Repl.Batteries (
  module System.Environment.Compat
) where
import "this" System.Environment.Compat
