module GHC.Stack (
#if MIN_VERSION_base(4,4,0)
  module Base
) where
import "base" GHC.Stack as Base
#else
) where
#endif
