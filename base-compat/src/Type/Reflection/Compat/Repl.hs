{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Type.Reflection.Compat"
-- from a globally unique namespace.
module Type.Reflection.Compat.Repl (
  module Type.Reflection.Compat
) where
import "this" Type.Reflection.Compat
