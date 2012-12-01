module Foreign.Safe (
#if MIN_VERSION_base(4,4,0)
  module Base
) where
import "base" Foreign.Safe as Base
#else
) where
#endif
