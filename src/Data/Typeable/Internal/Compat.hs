module Data.Typeable.Internal.Compat (
#if MIN_VERSION_base(4,4,0)
  module Base
) where
import "base" Data.Typeable.Internal as Base
#else
) where
#endif
