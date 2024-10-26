{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Control.Concurrent.Compat"
-- from a globally unique namespace.
module Control.Concurrent.Compat.Repl (
  module Control.Concurrent.Compat
) where
import "this" Control.Concurrent.Compat
