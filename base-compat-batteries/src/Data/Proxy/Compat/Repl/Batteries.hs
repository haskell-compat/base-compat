{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Data.Proxy.Compat"
-- from a globally unique namespace.
module Data.Proxy.Compat.Repl.Batteries (
  module Data.Proxy.Compat
) where
import "this" Data.Proxy.Compat
