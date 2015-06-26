{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.Proxy.Compat (
  Proxy         (..)
, asProxyTypeOf
) where

#if MIN_VERSION_base(4,7,0)
import Data.Proxy(Proxy(..), asProxyTypeOf)
#else

import GHC.Base
import GHC.Show
import GHC.Read
import GHC.Enum
import GHC.Arr

-- | A concrete, poly-kinded proxy type
data Proxy t = Proxy

-- We omit KProxy for older GHC versions.
--data KProxy (t :: *) = KProxy

instance Eq (Proxy s) where
  _ == _ = True

instance Ord (Proxy s) where
  compare _ _ = EQ

instance Show (Proxy s) where
  showsPrec _ _ = showString "Proxy"

instance Read (Proxy s) where
  readsPrec d = readParen (d > 10) (\r -> [(Proxy, s) | ("Proxy",s) <- lex r ])

instance Enum (Proxy s) where
    succ _               = error "Proxy.succ"
    pred _               = error "Proxy.pred"
    fromEnum _           = 0
    toEnum 0             = Proxy
    toEnum _             = error "Proxy.toEnum: 0 expected"
    enumFrom _           = [Proxy]
    enumFromThen _ _     = [Proxy]
    enumFromThenTo _ _ _ = [Proxy]
    enumFromTo _ _       = [Proxy]

instance Ix (Proxy s) where
    range _           = [Proxy]
    index _ _         = 0
    inRange _ _       = True
    rangeSize _       = 1
    unsafeIndex _ _   = 0
    unsafeRangeSize _ = 1

instance Bounded (Proxy s) where
    minBound = Proxy
    maxBound = Proxy

instance Functor Proxy where
    fmap _ _ = Proxy
    {-# INLINE fmap #-}

instance Monad Proxy where
    return _ = Proxy
    {-# INLINE return #-}
    _ >>= _ = Proxy
    {-# INLINE (>>=) #-}

-- | 'asProxyTypeOf' is a type-restricted version of 'const'.
-- It is usually used as an infix operator, and its typing forces its first
-- argument (which is usually overloaded) to have the same type as the tag
-- of the second.
asProxyTypeOf :: a -> Proxy a -> a
asProxyTypeOf = const
{-# INLINE asProxyTypeOf #-}
#endif

