module Control.Monad.ST.Lazy.Safe.Compat (
#if MIN_VERSION_base(4,4,0)
  module Base
) where
import "base" Control.Monad.ST.Lazy.Safe as Base
#else
) where
#endif
