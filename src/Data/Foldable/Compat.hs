{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Foldable.Compat (
  module Base
, Foldable(..)
#if !MIN_VERSION_base(4,8,0)
, length
, null
#endif
) where

import Data.Foldable as Base

#if !MIN_VERSION_base(4,7,0)
import Data.Either (Either(..))
import Data.Monoid (mempty)
import Control.Applicative (Const(..))
#endif

#if !MIN_VERSION_base(4,8,0)
import Prelude (Bool(..), Int, (+))

-- | Test whether the structure is empty. The default implementation is
-- optimized for structures that are similar to cons-lists, because there
-- is no general way to do better.
null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True

-- | Returns the size/length of a finite structure as an 'Int'.  The
-- default implementation is optimized for structures that are similar to
-- cons-lists, because there is no general way to do better.
length :: Foldable t => t a -> Int
length = foldl' (\c _ -> c+1) 0
#endif

#if !MIN_VERSION_base(4,7,0)
instance Foldable (Either a) where
    foldMap _ (Left _) = mempty
    foldMap f (Right y) = f y

    foldr _ z (Left _) = z
    foldr f z (Right y) = f y z

instance Foldable ((,) a) where
    foldMap f (_, y) = f y

    foldr f z (_, y) = f y z

instance Foldable (Const m) where
    foldMap _ _ = mempty
#endif
