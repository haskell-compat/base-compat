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
        Last(..),
        -- * Alternative wrapper
        Alt(..)
  ) where

import Data.Monoid as Base

-- To import orphan Generic instances for Sum and Product
import GHC.Generics.Compat ()

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
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
           , Monad, MonadPlus, Applicative, Alternative, Functor
# if __GLASGOW_HASKELL__ >= 702
           , Generic
# endif
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
           )

instance Alternative f => Monoid (Alt f a) where
    mempty = Alt empty
    mappend (Alt x) (Alt y) = Alt (x <|> y)
#endif
