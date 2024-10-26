{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Control.Monad.Compat"
-- from a globally unique namespace.
module Control.Monad.Compat.Repl (
  module Control.Monad.Compat
) where
import "this" Control.Monad.Compat
