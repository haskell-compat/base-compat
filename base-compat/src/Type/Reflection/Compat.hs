{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if MIN_VERSION_base(4,10,0)
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
# if !(MIN_VERSION_base(4,11,0))
{-# LANGUAGE TypeInType #-}
# endif
#endif
module Type.Reflection.Compat (
#if MIN_VERSION_base(4,10,0)
  module Base
, withTypeable
, pattern TypeRep
#endif
) where

#if MIN_VERSION_base(4,11,0)
import Type.Reflection as Base
#elif MIN_VERSION_base(4,10,0)
import Type.Reflection as Base hiding (withTypeable)
#endif

#if MIN_VERSION_base(4,10,0)
# if !(MIN_VERSION_base(4,11,0))
import GHC.Exts (TYPE)
import Type.Reflection (Typeable, TypeRep)
import Unsafe.Coerce (unsafeCoerce)

-- | Use a 'TypeRep' as 'Typeable' evidence.
withTypeable :: forall (a :: k) (r :: TYPE rep). ()
             => TypeRep a -> (Typeable a => r) -> r
withTypeable rep k = unsafeCoerce k' rep
  where k' :: Gift a r
        k' = Gift k

-- | A helper to satisfy the type checker in 'withTypeable'.
newtype Gift a (r :: TYPE rep) = Gift (Typeable a => r)
# endif

# if !(MIN_VERSION_base(4,17,0))
-- | A 'TypeableInstance' wraps up a 'Typeable' instance for explicit
-- handling. For internal use: for defining 'TypeRep' pattern.
data TypeableInstance (a :: k) where
 TypeableInstance :: Typeable a => TypeableInstance a

-- | Get a reified 'Typeable' instance from an explicit 'TypeRep'.
--
-- For internal use: for defining 'TypeRep' pattern.
typeableInstance :: forall a. TypeRep a -> TypeableInstance a
typeableInstance rep = withTypeable rep TypeableInstance

-- | A explicitly bidirectional pattern synonym to construct a
-- concrete representation of a type.
--
-- As an __expression__: Constructs a singleton @TypeRep a@ given a
-- implicit 'Typeable a' constraint:
--
-- @
-- TypeRep @a :: Typeable a => TypeRep a
-- @
--
-- As a __pattern__: Matches on an explicit @TypeRep a@ witness bringing
-- an implicit @Typeable a@ constraint into scope.
--
-- @
-- f :: TypeRep a -> ..
-- f TypeRep = {- Typeable a in scope -}
-- @
--
-- /Since: 4.17.0.0/
pattern TypeRep :: forall a. () => Typeable a => TypeRep a
pattern TypeRep <- (typeableInstance -> TypeableInstance)
  where TypeRep = typeRep
# endif
#endif
