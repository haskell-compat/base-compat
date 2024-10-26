{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
#if MIN_VERSION_base(4,10,0) && !(MIN_VERSION_base(4,19,0))
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
#endif
module Data.Typeable.Compat (
  module Base
#if MIN_VERSION_base(4,10,0)
, heqT
, decT
, hdecT
#endif
) where

import Data.Typeable as Base

#if MIN_VERSION_base(4,10,0) && !(MIN_VERSION_base(4,19,0))
import Prelude.Compat

import Data.Void (Void)
import qualified Type.Reflection.Compat as TR
#endif

#if MIN_VERSION_base(4,10,0)
# if !(MIN_VERSION_base(4,18,0))
-- | Extract a witness of heterogeneous equality of two types
--
-- /Since: 4.18.0.0/
heqT :: forall a b. (Typeable a, Typeable b) => Maybe (a :~~: b)
heqT = ta `TR.eqTypeRep` tb
  where
    ta = TR.typeRep :: TR.TypeRep a
    tb = TR.typeRep :: TR.TypeRep b
# endif

# if !(MIN_VERSION_base(4,19,0))
-- | Decide an equality of two types
--
-- /Since: 4.19.0.0/
decT :: forall a b. (Typeable a, Typeable b) => Either (a :~: b -> Void) (a :~: b)
decT = case hdecT @a @b of
  Right HRefl -> Right Refl
  Left p      -> Left (\Refl -> p HRefl)

-- | Decide heterogeneous equality of two types.
--
-- /Since: 4.19.0.0/
hdecT :: forall a b. (Typeable a, Typeable b) => Either (a :~~: b -> Void) (a :~~: b)
hdecT = ta `TR.decTypeRep` tb
  where
    ta = TR.typeRep :: TR.TypeRep a
    tb = TR.typeRep :: TR.TypeRep b
# endif
#endif
