{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Foldable.Compat (
  module Base
, Foldable(..)
) where
import Data.Foldable as Base

#if !MIN_VERSION_base(4,7,0)
import Data.Either (Either(..))
import Data.Monoid (mempty)
import Control.Applicative (Const(..))

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
