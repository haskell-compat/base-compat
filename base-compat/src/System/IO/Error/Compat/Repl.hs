{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "System.IO.Error.Compat"
-- from a globally unique namespace.
module System.IO.Error.Compat.Repl (
  module System.IO.Error.Compat
) where
import "this" System.IO.Error.Compat
