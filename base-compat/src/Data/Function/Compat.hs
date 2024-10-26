{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Function.Compat (
  module Base
, (&)
, applyWhen
) where
import Data.Function as Base

#if !(MIN_VERSION_base(4,18,0))
import Prelude.Compat
#endif

#if !(MIN_VERSION_base(4,18,0))
-- | 'applyWhen' applies a function to a value if a condition is true,
-- otherwise, it returns the value unchanged.
--
-- It is equivalent to @'flip' ('Data.Bool.bool' 'id')@.
--
-- Algebraic properties:
--
-- * @applyWhen 'True' = 'id'@
--
-- * @applyWhen 'False' f = 'id'@
--
-- /Since: 4.18.0.0/
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True  f x = f x
applyWhen False _ x = x
-- Proofs:
--
-- flip bool id = \q f -> bool id f q
-- = \f q -> case q of
--     True -> f = \x -> f x
--     False -> id = \x -> x ∎
--
-- applyWhen True = \f x -> f x
-- = \f -> \x -> f x = \f -> f = id ∎
--
-- applyWhen False f = \x -> x = id ∎
#endif
