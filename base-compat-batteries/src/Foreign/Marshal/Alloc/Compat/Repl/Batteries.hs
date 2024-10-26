{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Foreign.Marshal.Alloc.Compat"
-- from a globally unique namespace.
module Foreign.Marshal.Alloc.Compat.Repl.Batteries (
  module Foreign.Marshal.Alloc.Compat
) where
import "this" Foreign.Marshal.Alloc.Compat
