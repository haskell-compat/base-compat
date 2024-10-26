{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Foreign.ForeignPtr.Safe.Compat"
-- from a globally unique namespace.
module Foreign.ForeignPtr.Safe.Compat.Repl (
  module Foreign.ForeignPtr.Safe.Compat
) where
import "this" Foreign.ForeignPtr.Safe.Compat
