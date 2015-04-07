{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Ord.Compat (
  module Base
, Ord(..)
, Down(..)
) where
import Data.Ord as Base

#if !MIN_VERSION_base(4,7,0)
import Prelude.Compat
#endif

#if MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,7,0)
deriving instance Read a => Read (Down a)
deriving instance Show a => Show (Down a)
#elif !MIN_VERSION_base(4,6,0)
newtype Down a = Down a deriving (Eq, Show, Read)

instance Ord a => Ord (Down a) where
    compare (Down x) (Down y) = y `compare` x
#endif
