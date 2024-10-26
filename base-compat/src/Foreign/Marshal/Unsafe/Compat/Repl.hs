{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Foreign.Marshal.Unsafe.Compat"
-- from a globally unique namespace.
module Foreign.Marshal.Unsafe.Compat.Repl (
  module Foreign.Marshal.Unsafe.Compat
) where
import "this" Foreign.Marshal.Unsafe.Compat
