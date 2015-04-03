{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Version.Compat (
  module Base
, makeVersion
) where
import Data.Version as Base

#if !MIN_VERSION_base(4,7,0)
import Data.Data
#endif

#if !MIN_VERSION_base(4,8,0) && __GLASGOW_HASKELL__ >= 708
import GHC.Exts (IsList(..))
#endif

#if !MIN_VERSION_base(4,7,0)
versionConstr :: Constr
versionConstr = mkConstr versionDataType "Version" ["versionBranch","versionTags"] Prefix

versionDataType :: DataType
versionDataType = mkDataType "Data.Version.Version" [versionConstr]

instance Data Version where
  gfoldl k z (Version bs ts) = z Version `k` bs `k` ts
  toConstr (Version _ _) = versionConstr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (z Version))
                    _ -> error "Data.Data.gunfold(Version)"
  dataTypeOf _  = versionDataType
#endif

#if !MIN_VERSION_base(4,8,0)
-- | Construct tag-less 'Version'
--
-- /Since: 4.8.0.0/
makeVersion :: [Int] -> Version
makeVersion b = Version b []

# if __GLASGOW_HASKELL__ >= 708
-- | /Since: 4.8.0.0/
instance IsList Version where
  type (Item Version) = Int
  fromList = makeVersion
  toList = versionBranch
# endif

#endif
