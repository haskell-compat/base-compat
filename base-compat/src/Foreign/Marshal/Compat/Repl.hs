{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Foreign.Marshal.Compat"
-- from a globally unique namespace.
module Foreign.Marshal.Compat.Repl (
  module Foreign.Marshal.Compat
) where
import "this" Foreign.Marshal.Compat
