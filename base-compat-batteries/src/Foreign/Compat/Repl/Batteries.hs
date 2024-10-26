{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Foreign.Compat"
-- from a globally unique namespace.
module Foreign.Compat.Repl.Batteries (
  module Foreign.Compat
) where
import "this" Foreign.Compat
