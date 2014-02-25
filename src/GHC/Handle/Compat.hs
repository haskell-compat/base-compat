{-# OPTIONS_GHC -fno-warn-deprecations #-}
module GHC.Handle.Compat {-# DEPRECATED "use GHC.IO.Handle instead" #-} (module Base) where
import "base" GHC.Handle as Base
