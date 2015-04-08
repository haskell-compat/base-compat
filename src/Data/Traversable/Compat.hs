{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Traversable.Compat (
  module Base
, Traversable(..)
) where
import Data.Traversable as Base

#if !MIN_VERSION_base(4,7,0)
import Data.Either (Either(..))
import Data.Foldable.Compat ()
import Data.Function (($))
import Control.Applicative (Const(..),pure,(<$>))

instance Traversable (Either a) where
    traverse _ (Left x) = pure (Left x)
    traverse f (Right y) = Right <$> f y

instance Traversable ((,) a) where
    traverse f (x, y) = (,) x <$> f y

instance Traversable (Const m) where
    traverse _ (Const m) = pure $ Const m
#endif
