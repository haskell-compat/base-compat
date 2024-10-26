{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Word.Compat"
-- from a globally unique namespace.
module Data.Word.Compat.Repl (
  module Data.Word.Compat
) where
import "this" Data.Word.Compat
