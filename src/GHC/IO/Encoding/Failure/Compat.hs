module GHC.IO.Encoding.Failure.Compat (
#if MIN_VERSION_base(4,4,0)
  module Base
) where
import "base" GHC.IO.Encoding.Failure as Base
#else
) where
#endif
