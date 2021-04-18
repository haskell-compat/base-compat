{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Text.Read.Lex.Compat"
-- from a globally unique namespace.
module Text.Read.Lex.Compat.Repl.Batteries (
  module Text.Read.Lex.Compat
) where
import "this" Text.Read.Lex.Compat
