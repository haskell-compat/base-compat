{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Control.Monad.IO.Class.Compat"
-- from a globally unique namespace.
module Control.Monad.IO.Class.Compat.Repl.Batteries (
  module Control.Monad.IO.Class.Compat
) where
import "this" Control.Monad.IO.Class.Compat
