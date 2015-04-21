{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Functor.Compat (
  module Base
, Functor(..)
, ($>)
, void
) where
import Data.Functor as Base

#if !(MIN_VERSION_base(4,7,0))
import Control.Monad.Compat (void)
import Data.Function (flip)

infixl 4 $>

-- | Flipped version of '$>'.
--
-- /Since: 4.7.0.0/
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

#endif
