{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Foreign.ForeignPtr.Compat"
-- from a globally unique namespace.
module Foreign.ForeignPtr.Compat.Repl (
  module Foreign.ForeignPtr.Compat
) where
import "this" Foreign.ForeignPtr.Compat
