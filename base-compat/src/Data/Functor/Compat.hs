{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Functor.Compat (
  module Base
, Functor(..)
, ($>)
, void
, (<&>)
, unzip
) where
import Data.Functor as Base

#if !(MIN_VERSION_base(4,19,0))
import Data.Tuple (fst, snd)
#endif

#if !(MIN_VERSION_base(4,11,0))
-- | Flipped version of '<$>'.
--
-- @
-- ('<&>') = 'flip' 'fmap'
-- @
--
-- /Since: 4.11.0.0/
--
-- ==== __Examples__
-- Apply @(+1)@ to a list, a 'Data.Maybe.Just' and a 'Data.Either.Right':
--
-- >>> Just 2 <&> (+1)
-- Just 3
--
-- >>> [1,2,3] <&> (+1)
-- [2,3,4]
--
-- >>> Right 3 <&> (+1)
-- Right 4
--
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as

infixl 1 <&>
#endif

#if !(MIN_VERSION_base(4,19,0))
-- | Generalization of @Data.List.@'Data.List.unzip'.
--
-- /Since: 4.19.0.0/
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)
#endif
