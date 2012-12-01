module GHC.Event (
#if MIN_VERSION_base(4,4,0)
  module Base
) where
import "base" GHC.Event as Base
#else
) where
#endif
