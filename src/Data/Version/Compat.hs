{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Version.Compat (
  module Base
, makeVersion
) where
import Data.Version as Base

#if !MIN_VERSION_base(4,7,0)
import Data.Data
#endif

#if !MIN_VERSION_base(4,8,0)
import Prelude.Compat

# if MIN_VERSION_base(4,7,0)
import GHC.Exts (IsList(..))
# endif
#endif

#if !MIN_VERSION_base(4,7,0)
versionConstr :: Constr
versionConstr = mkConstr versionDataType "Version" ["versionBranch","versionTags"] Prefix

versionDataType :: DataType
versionDataType = mkDataType "Data.Version.Version" [versionConstr]

deriving instance Data Version
#endif

#if !MIN_VERSION_base(4,8,0)
-- | Construct tag-less 'Version'
--
-- /Since: 4.8.0.0/
makeVersion :: [Int] -> Version
makeVersion b = Version b []

# if MIN_VERSION_base(4,7,0)
-- | /Since: 4.8.0.0/
instance IsList Version where
  type (Item Version) = Int
  fromList = makeVersion
  toList = versionBranch
# endif

#endif
