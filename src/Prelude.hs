module Prelude (
  module Base
) where
#if MIN_VERSION_base(4,6,0)
import "base" Prelude as Base
#else
import "base" Prelude as Base hiding (catch)
#endif
