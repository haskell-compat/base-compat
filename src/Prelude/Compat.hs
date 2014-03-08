module Prelude.Compat (
  module Base
) where
#if MIN_VERSION_base(4,6,0)
import Prelude as Base
#else
import Prelude as Base hiding (catch)
#endif
