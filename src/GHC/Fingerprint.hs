module GHC.Fingerprint (
#if MIN_VERSION_base(4,4,0)
  module Base
) where
import "base" GHC.Fingerprint as Base
#else
) where
#endif
