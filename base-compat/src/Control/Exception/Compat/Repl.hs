{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Control.Exception.Compat"
-- from a globally unique namespace.
module Control.Exception.Compat.Repl (
  module Control.Exception.Compat
) where
import "this" Control.Exception.Compat
