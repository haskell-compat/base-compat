module Control.Monad.Zip.Compat (
#if MIN_VERSION_base(4,4,0)
  module Base
) where
import "base" Control.Monad.Zip as Base
#else
) where
#endif
