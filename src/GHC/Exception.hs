{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHC.Exception (
  module Base
) where
import "base" GHC.Exception as Base

#if __GLASGOW_HASKELL__ <= 706
import Prelude
deriving instance Eq ErrorCall
deriving instance Ord ErrorCall
#endif
