#if MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,7,0)
{-# LANGUAGE StandaloneDeriving #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Ord.Compat (
  module Base
) where
import Data.Ord as Base

#if MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,7,0)
import Prelude.Compat

deriving instance Read a => Read (Down a)
deriving instance Show a => Show (Down a)
#endif
