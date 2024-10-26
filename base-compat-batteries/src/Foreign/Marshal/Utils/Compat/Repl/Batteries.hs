{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Foreign.Marshal.Utils.Compat"
-- from a globally unique namespace.
module Foreign.Marshal.Utils.Compat.Repl.Batteries (
  module Foreign.Marshal.Utils.Compat
) where
import "this" Foreign.Marshal.Utils.Compat
