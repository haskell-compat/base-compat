module Control.Monad.Compat (
  module Base
, void
) where

import           Control.Monad as Base

#if !MIN_VERSION_base(4,3,0)

import           Prelude.Compat

-- | @'void' value@ discards or ignores the result of evaluation, such as the return value of an 'IO' action.
void :: Functor f => f a -> f ()
void = fmap (const ())
#endif
