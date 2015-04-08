{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Bool.Compat (
   -- * Booleans
   Bool(..),
   -- ** Operations
   (&&),
   (||),
   not,
   otherwise,
   bool,
  ) where

import Data.Bool

#if !MIN_VERSION_base(4,7,0)
import Data.Bits.Compat ()

-- | Case analysis for the 'Bool' type.
-- @bool a b p@ evaluates to @a@ when @p@ is @False@, and evaluates to @b@
-- when @p@ is @True@.
--
-- /Since: 4.7.0.0/
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t
#endif
