{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
module Data.Type.Coercion.Compat (
  module Base
, gcoerceWith
) where

import Data.Type.Coercion as Base

# if !(MIN_VERSION_base(4,10,0))
import Data.Coerce (Coercible)

-- | Generalized form of type-safe cast using representational equality
--
-- /Since: 4.10.0.0/
gcoerceWith :: Coercion a b -> (Coercible a b => r) -> r
gcoerceWith Coercion x = x
# endif
