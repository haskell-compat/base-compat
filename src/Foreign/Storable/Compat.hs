module Foreign.Storable.Compat (
  module Base
, Storable(..)
) where
import Foreign.Storable as Base

#if !MIN_VERSION_base(4,8,0)
import Data.Complex (Complex(..))
import Data.Ratio (Ratio(..))

instance Storable a => Storable (Complex a) where
    sizeOf a       = 2 * sizeOf (realPart a)
    alignment a    = alignment (realPart a)
    peek p           = do
                        q <- return $ castPtr p
                        r <- peek q
                        i <- peekElemOff q 1
                        return (r :+ i)
    poke p (r :+ i)  = do
                        q <-return $  (castPtr p)
                        poke q r
                        pokeElemOff q 1 i

instance (Storable a, Integral a) => Storable (Ratio a) where
    sizeOf _    = 2 * sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a )
    peek p           = do
                        q <- return $ castPtr p
                        r <- peek q
                        i <- peekElemOff q 1
                        return (r % i)
    poke p (r :% i)  = do
                        q <-return $  (castPtr p)
                        poke q r
                        pokeElemOff q 1 i
#endif