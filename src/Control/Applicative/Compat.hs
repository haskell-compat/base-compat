#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE StandaloneDeriving #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Applicative.Compat (
  module Base
, Const(..)
, WrappedMonad(..)
) where
import Control.Applicative as Base

#if !MIN_VERSION_base(4,7,0)
import Data.Monoid (Monoid(..),mempty,mappend)
import Prelude.Compat

-- Added in base-4.7.0.0
instance Monoid a => Monoid (Const a b) where
    mempty = Const mempty
    mappend (Const a) (Const b) = Const (mappend a b)

-- Added in base-4.7.0.0 (GHC Trac #8218)
instance Monad m => Monad (WrappedMonad m) where
    return = WrapMonad . return
    a >>= f = WrapMonad (unwrapMonad a >>= unwrapMonad . f)

deriving instance Eq a => Eq (ZipList a)
deriving instance Ord a => Ord (ZipList a)
deriving instance Read a => Read (ZipList a)
deriving instance Show a => Show (ZipList a)
#endif

#if !MIN_VERSION_base(4,8,0)
deriving instance Eq a => Eq (Const a b)
deriving instance Ord a => Ord (Const a b)

instance Read a => Read (Const a b) where
    readsPrec d = readParen (d > 10)
        $ \r -> [(Const x,t) | ("Const", s) <- lex r, (x, t) <- readsPrec 11 s]

instance Show a => Show (Const a b) where
    showsPrec d (Const x) = showParen (d > 10) $
                            showString "Const " . showsPrec 11 x
#endif
