{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Control.Monad.ST.Unsafe.Compat"
-- from a globally unique namespace.
module Control.Monad.ST.Unsafe.Compat.Repl (
  module Control.Monad.ST.Unsafe.Compat
) where
import "this" Control.Monad.ST.Unsafe.Compat
