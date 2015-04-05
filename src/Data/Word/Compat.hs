module Data.Word.Compat (
  module Base
, byteSwap16
, byteSwap32
, byteSwap64
) where
import Data.Word as Base

#if !MIN_VERSION_base(4,7,0)

# include "MachDeps.h"

import GHC.Prim
import GHC.Word

# if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
# endif

-- | Swap bytes in 'Word16'.
--
-- /Since: 4.7.0.0/
byteSwap16 :: Word16 -> Word16
byteSwap16 (W16# w#) = W16# (narrow16Word# (byteSwap16# w#))

-- | Reverse order of bytes in 'Word32'.
--
-- /Since: 4.7.0.0/
byteSwap32 :: Word32 -> Word32
byteSwap32 (W32# w#) = W32# (narrow32Word# (byteSwap32# w#))

-- | Reverse order of bytes in 'Word64'.
--
-- /Since: 4.7.0.0/
# if WORD_SIZE_IN_BITS < 64
byteSwap64 :: Word64 -> Word64
byteSwap64 (W64# w#) = W64# (byteSwap64# w#)
# else
byteSwap64 :: Word64 -> Word64
byteSwap64 (W64# w#) = W64# (byteSwap# w#)
# endif

#endif