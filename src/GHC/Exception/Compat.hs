{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHC.Exception.Compat (
  module Base
) where
import "base" GHC.Exception as Base
import Control.Exception.ErrorCall.EqInstance ()

#if __GLASGOW_HASKELL__ <= 706
import Prelude
deriving instance Ord ErrorCall
#endif
