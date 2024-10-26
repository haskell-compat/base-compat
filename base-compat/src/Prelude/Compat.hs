{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
#if MIN_VERSION_base(4,10,0) && !(MIN_VERSION_base(4,12,0))
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
#endif
#if MIN_VERSION_base(4,17,0) && !(MIN_VERSION_base(4,20,0))
{-# LANGUAGE ExplicitNamespaces #-}
#endif
module Prelude.Compat (
#if MIN_VERSION_base(4,20,0)
  module Base
#else
  either
, all
, and
, any
, concat
, concatMap
, mapM_
, notElem
, or
, sequence_
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
, errorWithoutStackTrace
, undefined
, seq

, elem
, foldMap
, foldl
, foldl'
, foldl1
, foldr
, foldr1
, length
, maximum
, minimum
, null
, product
, sum
, mapM
, sequence
, sequenceA
, traverse
, (*>)
, (<*)
, (<*>)
, pure
, liftA2
, (<$)
, fmap
, (>>)
, (>>=)
, fail
, return
, mappend
, mconcat
, mempty
, (<>)
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
, MonadFail
, Monoid
, Num (fromInteger)
, Ord
, Read
, Real
, RealFloat
, RealFrac
, Semigroup
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

# if MIN_VERSION_base(4,17,0)
-- The equality types
, type (~)
# endif
#endif
) where


import Prelude as Base hiding (
#if !(MIN_VERSION_base(4,13,0))
    fail
# if MIN_VERSION_base(4,10,0) && !(MIN_VERSION_base(4,12,0))
  , ($!)
# endif
#endif
  )

#if !(MIN_VERSION_base(4,18,0))
import Control.Applicative (liftA2)
#endif

#if !(MIN_VERSION_base(4,20,0))
import Data.Foldable (foldl')
#endif

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup as Base (Semigroup((<>)))
#endif

#if !(MIN_VERSION_base(4,13,0))
import Control.Monad.Fail as Base (MonadFail(fail))
#endif

#if MIN_VERSION_base(4,10,0) && !(MIN_VERSION_base(4,12,0))
import GHC.Exts (TYPE)
#endif

#if MIN_VERSION_base(4,10,0) && !(MIN_VERSION_base(4,12,0))
-- | Strict (call-by-value) application operator. It takes a function and an
-- argument, evaluates the argument to weak head normal form (WHNF), then calls
-- the function with that value.
infixr 0 $!
($!) :: forall r a (b :: TYPE r). (a -> b) -> a -> b
f $! x = let !vx = x in f vx  -- see #2273
#endif
