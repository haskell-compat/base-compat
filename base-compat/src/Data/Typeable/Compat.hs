{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if MIN_VERSION_base(4,10,0) && !(MIN_VERSION_base(4,18,0))
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
#endif
module Data.Typeable.Compat (
  module Base
#if MIN_VERSION_base(4,10,0)
, heqT
#endif
) where

import Data.Typeable as Base

#if MIN_VERSION_base(4,10,0) && !(MIN_VERSION_base(4,18,0))
import Prelude.Compat

import qualified Type.Reflection.Compat as TR
#endif

#if MIN_VERSION_base(4,10,0) && !(MIN_VERSION_base(4,18,0))
-- | Extract a witness of heterogeneous equality of two types
--
-- /Since: 4.18.0.0/
heqT :: forall a b. (Typeable a, Typeable b) => Maybe (a :~~: b)
heqT = ta `TR.eqTypeRep` tb
  where
    ta = TR.typeRep :: TR.TypeRep a
    tb = TR.typeRep :: TR.TypeRep b
#endif
