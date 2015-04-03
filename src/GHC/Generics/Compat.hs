#if !MIN_VERSION_base(4,7,0)
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
module GHC.Generics.Compat (
  module Base
) where
import GHC.Generics as Base

#if !MIN_VERSION_base(4,7,0)
import Control.Applicative
import Control.Arrow
import Data.Monoid

deriving instance Generic All
deriving instance Generic Any
deriving instance Generic (Const a b)
deriving instance Generic (Dual a)
deriving instance Generic (Endo a)
deriving instance Generic (First a)
deriving instance Generic (Last a)
deriving instance Generic (Product a)
deriving instance Generic (Sum a)
deriving instance Generic (WrappedArrow a b c)
deriving instance Generic (WrappedMonad m a)
deriving instance Generic (ZipList a)

deriving instance Generic1 (Const a)
deriving instance Generic1 Dual
deriving instance Generic1 First
deriving instance Generic1 Last
deriving instance Generic1 Product
deriving instance Generic1 Sum
deriving instance Generic1 (WrappedArrow a b)
deriving instance Generic1 (WrappedMonad m)
deriving instance Generic1 ZipList

deriving instance Eq (U1 p)
deriving instance Generic (U1 p)
deriving instance Ord (U1 p)
deriving instance Read (U1 p)
deriving instance Show (U1 p)

deriving instance Eq p => Eq (Par1 p)
deriving instance Generic (Par1 p)
deriving instance Ord p => Ord (Par1 p)
deriving instance Read p => Read (Par1 p)
deriving instance Show p => Show (Par1 p)

deriving instance Eq (f p) => Eq (Rec1 f p)
deriving instance Generic (Rec1 f p)
deriving instance Ord (f p) => Ord (Rec1 f p)
deriving instance Read (f p) => Read (Rec1 f p)
deriving instance Show (f p) => Show (Rec1 f p)

deriving instance Eq c => Eq (K1 i c p)
deriving instance Generic (K1 i c p)
deriving instance Ord c => Ord (K1 i c p)
deriving instance Read c => Read (K1 i c p)
deriving instance Show c => Show (K1 i c p)

deriving instance Eq (f p) => Eq (M1 i c p)
deriving instance Generic (M1 i c p)
deriving instance Ord (f p) => Ord (M1 i c p)
deriving instance Read (f p) => Read (M1 i c p)
deriving instance Show (f p) => Show (M1 i c p)

deriving instance (Eq (f p), Eq (g p)) => Eq ((f :+: g) p)
deriving instance Generic ((f :+: g) p)
deriving instance (Ord (f p), Ord (g p)) => Ord ((f :+: g) p)
deriving instance (Read (f p), Read (g p)) => Read ((f :+: g) p)
deriving instance (Show (f p), Show (g p)) => Show ((f :+: g) p)

deriving instance (Eq (f p), Eq (g p)) => Eq ((f :*: g) p)
deriving instance Generic ((f :*: g) p)
deriving instance (Ord (f p), Ord (g p)) => Ord ((f :*: g) p)
deriving instance (Read (f p), Read (g p)) => Read ((f :*: g) p)
deriving instance (Show (f p), Show (g p)) => Show ((f :*: g) p)

deriving instance Eq (f (g p)) => Eq ((f :.: g) p)
deriving instance Generic ((f :.: g) p)
deriving instance Ord (f (g p)) => Ord ((f :.: g) p)
deriving instance Read (f (g p)) => Read ((f :.: g) p)
deriving instance Show (f (g p)) => Show ((f :.: g) p)
#endif