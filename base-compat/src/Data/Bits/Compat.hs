{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns, PatternGuards #-}
module Data.Bits.Compat (
  module Base
, bitDefault
, testBitDefault
, popCountDefault
, (.^.)
, (.>>.)
, (.<<.)
, (!>>.)
, (!<<.)
, toIntegralSized
, oneBits
) where

import Data.Bits as Base

#if !(MIN_VERSION_base(4,17,0))
import Prelude
#endif

#if !(MIN_VERSION_base(4,17,0))
-- | Infix version of 'xor'.
--
-- /Since: 4.17/
(.^.) :: (Bits a) => a -> a -> a
(.^.) = xor

infixl 6 .^.

-- | Infix version of 'shiftR'.
--
-- /Since: 4.17/
(.>>.) :: (Bits a) => a -> Int -> a
(.>>.) = shiftR

infixl 8 .>>.

-- | Infix version of 'shiftL'.
--
-- /Since: 4.17/
(.<<.) :: (Bits a) => a -> Int -> a
(.<<.) = shiftL

infixl 8 .<<.

-- | Infix version of 'unsafeShiftR'.
--
-- /Since: 4.17/
(!>>.) :: (Bits a) => a -> Int -> a
(!>>.) = unsafeShiftR

infixl 8 !>>.

-- | Infix version of 'unsafeShiftL'.
--
-- /Since: 4.17/
(!<<.) :: (Bits a) => a -> Int -> a
(!<<.) = unsafeShiftL

infixl 8 !<<.
#endif

#if !(MIN_VERSION_base(4,16,0))
-- | A more concise version of @complement zeroBits@.
--
-- >>> complement (zeroBits :: Word) == (oneBits :: Word)
-- True
--
-- >>> complement (oneBits :: Word) == (zeroBits :: Word)
-- True
--
-- = Note
--
-- The constraint on 'oneBits' is arguably too strong. However, as some types
-- (such as 'Natural') have undefined 'complement', this is the only safe
-- choice.
--
-- /Since: 4.16/
oneBits :: (FiniteBits a) => a
oneBits = complement zeroBits
{-# INLINE oneBits #-}
#endif
