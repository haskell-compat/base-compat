module GHC.Conc.Signal.Compat (
#if MIN_VERSION_base(4,3,0)
  module Base
) where
import "base" GHC.Conc.Signal as Base
#else
) where
#endif
