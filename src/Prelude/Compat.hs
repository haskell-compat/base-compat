module Prelude.Compat (
#if MIN_VERSION_base(4,8,0)
  module Base
#else
  either
, Data.Foldable.all
, Data.Foldable.and
, Data.Foldable.any
, Data.Foldable.concat
, Data.Foldable.concatMap
, Data.Foldable.mapM_
, Data.Foldable.notElem
, Data.Foldable.or
, Data.Foldable.sequence_
, (<$>)
, maybe
, lines
, unlines
, unwords
, words
, curry
, fst
, snd
, uncurry
, ($!)
, (++)
, (.)
, (=<<)
, asTypeOf
, const
, flip
, id
, map
, otherwise
, until
, ioError
, userError
, (!!)
, break
, cycle
, drop
, dropWhile
, filter
, head
, init
, iterate
, last
, lookup
, repeat
, replicate
, reverse
, scanl
, scanl1
, scanr
, scanr1
, span
, splitAt
, tail
, take
, takeWhile
, unzip
, unzip3
, zip
, zip3
, zipWith
, zipWith3
, subtract
, lex
, readParen
, (^)
, (^^)

, even
, fromIntegral
, gcd
, lcm
, odd
, realToFrac
, showChar
, showParen
, showString
, shows
, appendFile
, getChar
, getContents
, getLine
, interact
, print
, putChar
, putStr
, putStrLn
, readFile
, readIO
, readLn
, writeFile
, read
, reads
, (&&)
, not
, (||)
, ($)
, error
, undefined
, seq

, Data.Foldable.elem
, foldMap
, Data.Foldable.foldl
, Data.Foldable.foldl1
, foldr
, Data.Foldable.foldr1
, length
, Data.Foldable.maximum
, Data.Foldable.minimum
, null
, Data.Foldable.product
, Data.Foldable.sum
, mapM
, sequence
, sequenceA
, traverse
, (*>)
, (<*)
, (<*>)
, pure
, (<$)
, fmap
, (>>)
, (>>=)
, fail
, return
, mappend
, mconcat
, mempty
, maxBound
, minBound
, enumFrom
, enumFromThen
, enumFromThenTo
, enumFromTo
, fromEnum
, pred
, succ
, toEnum
, (**)
, acos
, acosh
, asin
, asinh
, atan
, atanh
, cos
, cosh
, exp
, log
, logBase
, pi
, sin
, sinh
, sqrt
, tan
, tanh
, atan2
, decodeFloat
, encodeFloat
, exponent
, floatDigits
, floatRadix
, floatRange
, isDenormalized
, isIEEE
, isInfinite
, isNaN
, isNegativeZero
, scaleFloat
, significand
, (*)
, (+)
, (-)
, abs
, negate
, signum
, readList
, readsPrec
, (/)
, fromRational
, recip
, div
, divMod
, mod
, quot
, quotRem
, rem
, toInteger
, toRational
, ceiling
, floor
, properFraction
, round
, truncate
, show
, showList
, showsPrec
, (/=)
, (==)
, (<)
, (<=)
, (>)
, (>=)
, compare
, max
, min

-- classes
, Applicative
, Bounded
, Enum
, Eq
, Floating
, Foldable
, Fractional
, Functor
, Integral
, Monad
, Monoid
, Num (fromInteger)
, Ord
, Read
, Real
, RealFloat
, RealFrac
, Show
, Traversable

-- data types
, IO
, Char
, Double
, Float
, Int
, Integer
, Word
, Bool (True, False)
, Either(Left, Right)
, Maybe(Just, Nothing)
, Ordering (EQ, GT, LT)

-- type synonyms
, FilePath
, IOError
, Rational
, ReadS
, ShowS
, String
#endif
) where


#if MIN_VERSION_base(4,8,0)

import Prelude as Base

#else

import Prelude hiding (length, null, foldr, mapM, sequence)

import Data.Word
import Data.Foldable
import Data.Traversable.Compat
import Data.Monoid
import Control.Applicative

-- | Test whether the structure is empty. The default implementation is
-- optimized for structures that are similar to cons-lists, because there
-- is no general way to do better.
null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True

-- | Returns the size/length of a finite structure as an 'Int'.  The
-- default implementation is optimized for structures that are similar to
-- cons-lists, because there is no general way to do better.
length :: Foldable t => t a -> Int
length = foldl' (\c _ -> c+1) 0
#endif
