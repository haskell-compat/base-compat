#if MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,7,0)
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
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

-- These instances are only valid if Bits isn't a subclass of Num (as Bool is
-- not a Num instance), which is only true as of base-4.6.0.0 and later.
# if MIN_VERSION_base(4,6,0)
import Data.Bits (Bits(..))
import Data.Eq ((/=))

instance Bits Bool where
    (.&.) = (&&)

    (.|.) = (||)

    xor = (/=)

    complement = not

    shift x 0 = x
    shift _ _ = False

    rotate x _ = x

    bit 0 = True
    bit _ = False

    testBit x 0 = x
    testBit _ _ = False

    bitSize _ = 1

    isSigned _ = False

    popCount False = 0
    popCount True  = 1
# endif

-- | Case analysis for the 'Bool' type.
-- @bool a b p@ evaluates to @a@ when @p@ is @False@, and evaluates to @b@
-- when @p@ is @True@.
--
-- /Since: 4.7.0.0/
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t
#endif
