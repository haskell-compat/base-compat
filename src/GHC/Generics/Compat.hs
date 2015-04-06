{-# LANGUAGE FlexibleContexts, DeriveGeneric, StandaloneDeriving, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHC.Generics.Compat (
-- GHC.Generics is only available on GHC 7.2 and later, so we can't export anything
-- if we're using a GHC prior to 7.2
#if __GLASGOW_HASKELL__ < 702
) where
#else
  module Base
) where
import           GHC.Generics as Base

# if !MIN_VERSION_base(4,7,0)
import           GHC.Read
import           Prelude.Compat
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import           Text.Read.Lex

-- Although DeriveGeneric has been around since GHC 7.2, various bugs cause
-- the standalone-derived code below to fail to compile unless a fairly
-- recent version of GHC is used.
#  if __GLASGOW_HASKELL__ >= 706
import           Control.Applicative
import           Data.Monoid

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

deriving instance Generic (U1 p)
deriving instance Generic (Par1 p)
deriving instance Generic (Rec1 f p)
deriving instance Generic (K1 i c p)
deriving instance Generic (M1 i c f p)
deriving instance Generic ((f :+: g) p)
deriving instance Generic ((f :*: g) p)
deriving instance Generic ((f :.: g) p)
#  endif

deriving instance Eq (U1 p)
deriving instance Ord (U1 p)
deriving instance Read (U1 p)
deriving instance Show (U1 p)

deriving instance Eq p => Eq (Par1 p)
deriving instance Ord p => Ord (Par1 p)
deriving instance Read p => Read (Par1 p)
deriving instance Show p => Show (Par1 p)

deriving instance Eq (f p) => Eq (Rec1 f p)
deriving instance Ord (f p) => Ord (Rec1 f p)
deriving instance Read (f p) => Read (Rec1 f p)
deriving instance Show (f p) => Show (Rec1 f p)

deriving instance Eq c => Eq (K1 i c p)
deriving instance Ord c => Ord (K1 i c p)
deriving instance Read c => Read (K1 i c p)
deriving instance Show c => Show (K1 i c p)

deriving instance Eq (f p) => Eq (M1 i c f p)
deriving instance Ord (f p) => Ord (M1 i c f p)
deriving instance Read (f p) => Read (M1 i c f p)
deriving instance Show (f p) => Show (M1 i c f p)

deriving instance (Eq (f p), Eq (g p)) => Eq ((f :+: g) p)
deriving instance (Ord (f p), Ord (g p)) => Ord ((f :+: g) p)
deriving instance (Read (f p), Read (g p)) => Read ((f :+: g) p)
deriving instance (Show (f p), Show (g p)) => Show ((f :+: g) p)

deriving instance (Eq (f p), Eq (g p)) => Eq ((f :*: g) p)
deriving instance (Ord (f p), Ord (g p)) => Ord ((f :*: g) p)
-- Due to a GHC bug (https://ghc.haskell.org/trac/ghc/ticket/9830), the derived
-- Read and Show instances for infix data constructors will use the wrong
-- precedence (prior to GHC 7.10).
-- We'll manually derive Read :*: and Show :*: instances to avoid this.
instance (Read (f p), Read (g p)) => Read ((f :*: g) p) where
    readPrec = parens . ReadPrec.prec 6 $ do
        fp <- ReadPrec.step readPrec
        Symbol ":*:" <- lexP
        gp <- ReadPrec.step readPrec
        return $ fp :*: gp
    readListPrec = readListPrecDefault
instance (Show (f p), Show (g p)) => Show ((f :*: g) p) where
     showsPrec p (l :*: r) = showParen (p > sixPrec) $
            showsPrec (sixPrec + 1) l
         . showString " :*: "
         . showsPrec (sixPrec + 1) r
       where sixPrec = 6

deriving instance Eq (f (g p)) => Eq ((f :.: g) p)
deriving instance Ord (f (g p)) => Ord ((f :.: g) p)
deriving instance Read (f (g p)) => Read ((f :.: g) p)
deriving instance Show (f (g p)) => Show ((f :.: g) p)
# endif

#endif
