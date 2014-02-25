module GHC.Conc.Sync.Compat (
#if MIN_VERSION_base(4,3,0)
  module Base
) where
import "base" GHC.Conc.Sync as Base
#else
) where
#endif
