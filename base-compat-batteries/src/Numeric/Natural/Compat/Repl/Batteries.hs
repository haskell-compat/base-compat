{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- | Reexports "Numeric.Natural.Compat"
-- from a globally unique namespace.
module Numeric.Natural.Compat.Repl.Batteries (
  module Numeric.Natural.Compat
) where
import "this" Numeric.Natural.Compat
