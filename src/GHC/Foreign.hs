module GHC.Foreign (
#if MIN_VERSION_base(4,4,0)
  module Base
) where
import "base" GHC.Foreign as Base
#else
) where
#endif
