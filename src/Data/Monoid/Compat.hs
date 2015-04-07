{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

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
        Last(..),
        -- * Alternative wrapper
        Alt(..)
  ) where

import Data.Monoid as Base

-- To import orphan Generic instances for Sum and Product
import GHC.Generics.Compat ()

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Control.Monad
import Prelude.Compat

# if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
# endif
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

#if !MIN_VERSION_base(4,8,0)
-- | Monoid under '<|>'.
--
-- /Since: 4.8.0.0/
newtype Alt f a = Alt {getAlt :: f a}
  deriving ( Read, Show, Eq, Ord, Num, Enum
# if __GLASGOW_HASKELL__ >= 702
           , Generic
# endif
# if __GLASGOW_HASKELL__ >= 708
           , Generic1
# endif
           )

-- To work around a GHC 7.6 bug, we'll use StandaloneDeriving for generalized
-- derivations that involve higher-kinded typeclasses.
deriving instance Functor f => Functor (Alt f)
deriving instance Applicative f => Applicative (Alt f)
deriving instance Monad m => Monad (Alt m)
deriving instance Alternative f => Alternative (Alt f)
deriving instance MonadPlus m => MonadPlus (Alt m)

instance Alternative f => Monoid (Alt f a) where
    mempty = Alt empty
    mappend (Alt x) (Alt y) = Alt (x <|> y)
#endif
