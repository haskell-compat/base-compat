{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Text.Read.Compat"
-- from a globally unique namespace.
module Text.Read.Compat.Repl (
  module Text.Read.Compat
) where
import "this" Text.Read.Compat
