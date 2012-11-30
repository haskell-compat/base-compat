{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Read
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (uses Text.ParserCombinators.ReadP)
--
-- Converting strings to values.
--
-- The "Text.Read" library is the canonical library to import for
-- 'Read'-class facilities.  For GHC only, it offers an extended and much
-- improved 'Read' class, which constitutes a proposed alternative to the
-- Haskell 98 'Read'.  In particular, writing parsers is easier, and
-- the parsers are much more efficient.
--
-----------------------------------------------------------------------------
module Text.Read.Compat (
   -- * The 'Read' class
   Read(..),
   ReadS,

   -- * Haskell 98 functions
   reads,
   read,
   readParen,
   lex,

   -- * New parsing functions
   module Text.ParserCombinators.ReadPrec,
   Lexeme(..),
   lexP,
   parens,
   readListDefault,
   readListPrecDefault,
   readEither,
   readMaybe
) where

import Text.Read
import Text.ParserCombinators.ReadPrec

#if !MIN_VERSION_base(4,6,0)
import qualified Text.ParserCombinators.ReadP as P

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
-- A 'Left' value indicates a parse error.
readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift P.skipSpaces
       return x

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case readEither s of
                Left _  -> Nothing
                Right a -> Just a
#endif
