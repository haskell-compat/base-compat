module GHC.Float.RealFracMethods.Compat (
#if MIN_VERSION_base(4,4,0)
  module Base
) where
import "base" GHC.Float.RealFracMethods as Base
#else
) where
#endif
