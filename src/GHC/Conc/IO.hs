module GHC.Conc.IO (
#if MIN_VERSION_base(4,3,0)
  module Base
) where
import "base" GHC.Conc.IO as Base
#else
) where
#endif
