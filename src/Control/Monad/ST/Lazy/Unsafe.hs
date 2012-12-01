{-# LANGUAGE PackageImports #-}
module Control.Monad.ST.Lazy.Unsafe (
#if MIN_VERSION_base(4,4,0)
  module Base
) where
import "base" Control.Monad.ST.Lazy.Unsafe as Base
#else
) where
#endif
