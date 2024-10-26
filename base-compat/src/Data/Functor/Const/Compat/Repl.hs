{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Functor.Const.Compat"
-- from a globally unique namespace.
module Data.Functor.Const.Compat.Repl (
  module Data.Functor.Const.Compat
) where
import "this" Data.Functor.Const.Compat
