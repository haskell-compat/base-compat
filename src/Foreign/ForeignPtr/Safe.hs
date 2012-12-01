module Foreign.ForeignPtr.Safe (
#if MIN_VERSION_base(4,4,0)
  module Base
) where
import "base" Foreign.ForeignPtr.Safe as Base
#else
) where
#endif
