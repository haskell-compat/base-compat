module GHC.Stack.Compat (
#if MIN_VERSION_base(4,5,0)
  module Base
) where
import "base" GHC.Stack as Base
#else
) where
#endif
