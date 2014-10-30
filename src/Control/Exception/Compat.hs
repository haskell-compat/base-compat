{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Exception.Compat (
  module Base
) where

import Control.Exception as Base

#if __GLASGOW_HASKELL__ <= 706
import Prelude
deriving instance Ord ErrorCall
deriving instance Eq ErrorCall
#endif
