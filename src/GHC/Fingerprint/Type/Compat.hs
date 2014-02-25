module GHC.Fingerprint.Type.Compat (
#if MIN_VERSION_base(4,4,0)
  module Base
) where
import "base" GHC.Fingerprint.Type as Base
#else
) where
#endif
