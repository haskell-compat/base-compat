{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "System.Exit.Compat"
-- from a globally unique namespace.
module System.Exit.Compat.Repl.Batteries (
  module System.Exit.Compat
) where
import "this" System.Exit.Compat
