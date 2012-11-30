{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A class for monoids (types with an associative binary operation that
-- has an identity) with various general-purpose instances.
--
-----------------------------------------------------------------------------
module Data.Monoid.Compat (
  -- * Monoid typeclass
  Monoid(..),
  (<>),
  Dual(..),
  Endo(..),
  -- * Bool wrappers
  All(..),
  Any(..),
  -- * Num wrappers
  Sum(..),
  Product(..),
  -- * Maybe wrappers
  -- $MaybeExamples
  First(..),
  Last(..)
) where

import Data.Monoid

#if !MIN_VERSION_base(4,5,0)
-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

-- $MaybeExamples
-- To implement @find@ or @findLast@ on any 'Foldable':
--
-- @
-- findLast :: Foldable t => (a -> Bool) -> t a -> Maybe a
-- findLast pred = getLast . foldMap (\x -> if pred x
--                                            then Last (Just x)
--                                            else Last Nothing)
-- @
--
-- Much of Data.Map's interface can be implemented with
-- Data.Map.alter. Some of the rest can be implemented with a new
-- @alterA@ function and either 'First' or 'Last':
--
-- > alterA :: (Applicative f, Ord k) =>
-- >           (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
-- >
-- > instance Monoid a => Applicative ((,) a)  -- from Control.Applicative
--
-- @
-- insertLookupWithKey :: Ord k => (k -> v -> v -> v) -> k -> v
--                     -> Map k v -> (Maybe v, Map k v)
-- insertLookupWithKey combine key value =
--   Arrow.first getFirst . alterA doChange key
--   where
--   doChange Nothing = (First Nothing, Just value)
--   doChange (Just oldValue) =
--     (First (Just oldValue),
--      Just (combine key value oldValue))
-- @
