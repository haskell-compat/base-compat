{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Data.Type.Coercion.Compat (
#if MIN_VERSION_base(4,7,0)
  module Base
, gcoerceWith
#endif
) where

#if MIN_VERSION_base(4,7,0)
import Data.Type.Coercion as Base

import Data.Coerce (Coercible)

# if !(MIN_VERSION_base(4,10,0))
-- | Generalized form of type-safe cast using representational equality
--
-- /Since: 4.10.0.0/
gcoerceWith :: Coercion a b -> (Coercible a b => r) -> r
gcoerceWith Coercion x = x
# endif
#endif
