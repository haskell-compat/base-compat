{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Numeric.Compat (
  module Base
, showBin
, showFFloatAlt
, showGFloatAlt
, showHFloat
, readBin
) where

import Numeric as Base

#if !(MIN_VERSION_base(4,16,0))
import Data.Char (intToDigit)
import Prelude
import Text.ParserCombinators.ReadP (readP_to_S)
import qualified Text.Read.Lex.Compat as L
#endif

#if !(MIN_VERSION_base(4,11,0))
{- | Show a floating-point value in the hexadecimal format,
similar to the @%a@ specifier in C's printf.

  >>> showHFloat (212.21 :: Double) ""
  "0x1.a86b851eb851fp7"
  >>> showHFloat (-12.76 :: Float) ""
  "-0x1.9851ecp3"
  >>> showHFloat (-0 :: Double) ""
  "-0x0p+0"
-}
showHFloat :: RealFloat a => a -> ShowS
showHFloat = showString . fmt
  where
  fmt x
    | isNaN x                   = "NaN"
    | isInfinite x              = (if x < 0 then "-" else "") ++ "Infinity"
    | x < 0 || isNegativeZero x = '-' : cvt (-x)
    | otherwise                 = cvt x

  cvt x
    | x == 0 = "0x0p+0"
    | otherwise =
      case floatToDigits 2 x of
        r@([], _) -> error $ "Impossible happened: showHFloat: " ++ show r
        (d:ds, e) -> "0x" ++ show d ++ frac ds ++ "p" ++ show (e-1)

  -- Given binary digits, convert them to hex in blocks of 4
  -- Special case: If all 0's, just drop it.
  frac digits
    | allZ digits = ""
    | otherwise   = "." ++ hex digits
    where
    hex ds =
      case ds of
        []                -> ""
        [a]               -> hexDigit a 0 0 0 ""
        [a,b]             -> hexDigit a b 0 0 ""
        [a,b,c]           -> hexDigit a b c 0 ""
        a : b : c : d : r -> hexDigit a b c d (hex r)

  hexDigit a b c d = showHex (8*a + 4*b + 2*c + d)

  allZ xs = case xs of
              x : more -> x == 0 && allZ more
              []       -> True
#endif

#if !(MIN_VERSION_base(4,16,0))
-- | Read an unsigned number in binary notation.
--
-- >>> readBin "10011"
-- [(19,"")]
readBin :: (Eq a, Num a) => ReadS a
readBin = readP_to_S L.readBinP

-- | Show /non-negative/ 'Integral' numbers in base 2.
showBin :: (Integral a, Show a) => a -> ShowS
showBin = showIntAtBase 2  intToDigit
#endif
