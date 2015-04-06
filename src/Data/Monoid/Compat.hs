{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import Data.Monoid as Base

#if !MIN_VERSION_base(4,7,0)
import GHC.Generics.Compat ()
import Prelude.Compat (Num)
#endif

#if !MIN_VERSION_base(4,5,0)
infixr 6 <>

-- | An infix synonym for 'mappend'.
--
-- /Since: 4.5.0.0/
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

#if !MIN_VERSION_base(4,7,0)
deriving instance Num a => Num (Sum a)
deriving instance Num a => Num (Product a)
#endif
