{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Applicative.Compat (
  module Base
, Const(..)
, WrappedMonad(..)
) where
import "base" Control.Applicative as Base

#if !MIN_VERSION_base(4,7,0)
import Data.Monoid (Monoid(..),mempty,mappend)
import Control.Monad (Monad(..))
import Data.Function ((.))
-- Added in base-4.7.0.0
instance Monoid a => Monoid (Const a b) where
    mempty = Const mempty
    mappend (Const a) (Const b) = Const (mappend a b)

-- Added in base-4.7.0.0 (GHC Trac #8218)
instance Monad m => Monad (WrappedMonad m) where
    return = WrapMonad . return
    a >>= f = WrapMonad (unwrapMonad a >>= unwrapMonad . f)
#endif
