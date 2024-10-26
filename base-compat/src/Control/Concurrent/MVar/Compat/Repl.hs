{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Control.Concurrent.MVar.Compat"
-- from a globally unique namespace.
module Control.Concurrent.MVar.Compat.Repl (
  module Control.Concurrent.MVar.Compat
) where
import "this" Control.Concurrent.MVar.Compat
