{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Control.Concurrent.QSem.Compat {-# DEPRECATED "Control.Concurrent.QSem will be removed in GHC 7.8. Please use an alternative, e.g. the SafeSemaphore package, instead." #-} (module Base) where
import Control.Concurrent.QSem as Base
