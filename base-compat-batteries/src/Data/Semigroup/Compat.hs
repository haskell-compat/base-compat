{-# LANGUAGE NoImplicitPrelude, PackageImports #-}
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

import "base-compat" Data.Semigroup.Compat
