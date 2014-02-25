{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Control.Concurrent.SampleVar.Compat {-# DEPRECATED "Control.Concurrent.SampleVar will be removed in GHC 7.8. Please use an alternative, e.g. the SafeSemaphore package, instead." #-} (module Base) where
import "base" Control.Concurrent.SampleVar as Base
