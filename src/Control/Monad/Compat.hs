module Control.Monad.Compat (
  module Base
, void
, (<$!>)
) where
import Control.Monad as Base

#if !MIN_VERSION_base(4,3,0)
import Data.Function (const)
import Data.Functor (Functor(..))
#endif

#if !MIN_VERSION_base(4,8,0)
import Prelude.Compat (seq)
#endif

#if !MIN_VERSION_base(4,3,0)
-- | @'void' value@ discards or ignores the result of evaluation, such as the return value of an 'IO' action.
void :: Functor f => f a -> f ()
void = fmap (const ())
#endif

#if !MIN_VERSION_base(4,8,0)
-- | Strict version of 'Data.Functor.<$>'.
--
-- /Since: 4.8.0.0/
(<$!>) :: Monad m => (a -> b) -> m a -> m b
{-# INLINE (<$!>) #-}
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z
#endif
