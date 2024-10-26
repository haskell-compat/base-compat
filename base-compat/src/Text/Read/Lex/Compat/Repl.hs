{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Text.Read.Lex.Compat"
-- from a globally unique namespace.
module Text.Read.Lex.Compat.Repl (
  module Text.Read.Lex.Compat
) where
import "this" Text.Read.Lex.Compat
