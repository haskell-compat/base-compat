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

# if MIN_VERSION_base(4,5,0)
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
