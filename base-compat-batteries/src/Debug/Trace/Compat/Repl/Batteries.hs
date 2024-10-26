{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Debug.Trace.Compat"
-- from a globally unique namespace.
module Debug.Trace.Compat.Repl.Batteries (
  module Debug.Trace.Compat
) where
import "this" Debug.Trace.Compat
