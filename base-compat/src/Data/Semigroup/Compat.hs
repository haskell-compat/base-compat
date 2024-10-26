{-# LANGUAGE NoImplicitPrelude #-}
-- | This backports the modern "Data.Semigroup" interface back to
-- @base-4.9@/GHC 8.0.
module Data.Semigroup.Compat (
    Semigroup(..)
  , stimesMonoid
  , stimesIdempotent
  , stimesIdempotentMonoid
  , mtimesDefault
  -- * Semigroups
  , Min(..)
  , Max(..)
  , First(..)
  , Last(..)
  , WrappedMonoid(..)
  -- * Re-exported monoids from Data.Monoid
  , Dual(..)
  , Endo(..)
  , All(..)
  , Any(..)
  , Sum(..)
  , Product(..)
  -- * Difference lists of a semigroup
  , diff
  , cycle1
  -- * ArgMin, ArgMax
  , Arg(..)
  , ArgMin
  , ArgMax
  ) where

import Data.Semigroup
