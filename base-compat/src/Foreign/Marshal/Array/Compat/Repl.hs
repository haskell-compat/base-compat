{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Foreign.Marshal.Array.Compat"
-- from a globally unique namespace.
module Foreign.Marshal.Array.Compat.Repl (
  module Foreign.Marshal.Array.Compat
) where
import "this" Foreign.Marshal.Array.Compat
