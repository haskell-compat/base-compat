{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Exception.Compat (
  module Base
, ErrorCall(..)
) where

import Control.Exception as Base

#if __GLASGOW_HASKELL__ <= 706
import Prelude.Compat
deriving instance Ord ErrorCall
deriving instance Eq ErrorCall
#endif
