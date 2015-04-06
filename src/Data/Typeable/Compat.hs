#if __GLASGOW_HASKELL__ > 704 && !MIN_VERSION_base(4,7,0)
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
module Data.Typeable.Compat (
  module Base
) where
import Data.Typeable as Base

#if __GLASGOW_HASKELL__ > 704 && !MIN_VERSION_base(4,7,0)
import GHC.Prim
 
deriving instance Typeable (->)
deriving instance Typeable RealWorld
#endif
