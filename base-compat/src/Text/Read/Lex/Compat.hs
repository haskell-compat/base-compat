{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
module Text.Read.Lex.Compat (
  module Base
, readBinP
) where

import Text.Read.Lex as Base

#if !(MIN_VERSION_base(4,16,0))
import Data.Char (ord)
import Prelude
import Text.ParserCombinators.ReadP (ReadP)
#endif

#if !(MIN_VERSION_base(4,16,0))
readBinP :: (Eq a, Num a) => ReadP a
readBinP = readIntP'2
{-# SPECIALISE readBinP :: ReadP Integer #-}

readIntP'2 :: (Eq a, Num a) => ReadP a
readIntP'2 = readIntP 2 isDigit valDigit
 where
  isDigit  c = maybe False (const True) (valDig2 c)
  valDigit c = maybe 0     id           (valDig2 c)
{-# SPECIALISE readIntP'2 :: ReadP Integer #-}

valDig2 :: Char -> Maybe Int
valDig2 c
  | '0' <= c && c <= '1' = Just (ord c - ord '0')
  | otherwise            = Nothing
#endif
