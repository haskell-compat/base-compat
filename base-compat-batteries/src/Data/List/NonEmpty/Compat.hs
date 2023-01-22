{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.List.NonEmpty.Compat (
  -- * The type of non-empty streams
    NonEmpty(..)

  -- * Non-empty stream transformations
  , map
  , intersperse
  , scanl
  , scanr
  , scanl1
  , scanr1
  , transpose
  , sortBy
  , sortWith
  -- * Basic functions
  , length
  , head
  , tail
  , last
  , init
  , singleton
  , (<|), cons
  , uncons
  , unfoldr
  , sort
  , reverse
  , inits
  , inits1
  , tails
  , tails1
  -- * Building streams
  , iterate
  , repeat
  , cycle
  , unfold
  , insert
  , some1
  -- * Extracting sublists
  , take
  , drop
  , splitAt
  , takeWhile
  , dropWhile
  , span
  , break
  , filter
  , partition
  , group
  , groupBy
  , groupWith
  , groupAllWith
  , group1
  , groupBy1
  , groupWith1
  , groupAllWith1
  -- * Sublist predicates
  , isPrefixOf
  -- * \"Set\" operations
  , nub
  , nubBy
  -- * Indexing streams
  , (!!)
  -- * Zipping and unzipping streams
  , zip
  , zipWith
  , unzip
  -- * Converting to and from a list
  , fromList
  , toList
  , nonEmpty
  , xor
) where

#if MIN_VERSION_base(4,9,0)
import "base-compat" Data.List.NonEmpty.Compat
#else
import "semigroups" Data.List.NonEmpty

import qualified "this" Prelude.Compat as Prelude
import           "this" Prelude.Compat ((.))

import qualified "this" Data.Foldable.Compat as Foldable
import qualified "this" Data.List.Compat as List
#endif

#if !(MIN_VERSION_base(4,9,0))
-- | Construct a 'NonEmpty' list from a single element.
--
-- /Since: 4.15/
singleton :: a -> NonEmpty a
singleton a = a :| []

-- | The 'inits1' function takes a 'NonEmpty' stream @xs@ and returns all the
-- 'NonEmpty' finite prefixes of @xs@, starting with the shortest.
--
-- > inits1 (1 :| [2,3]) == (1 :| []) :| [1 :| [2], 1 :| [2,3]]
-- > inits1 (1 :| []) == (1 :| []) :| []
--
-- /Since: 4.18/
inits1 :: NonEmpty a -> NonEmpty (NonEmpty a)
inits1 =
  -- fromList is an unsafe function, but this usage should be safe, since:
  -- - `inits xs = [[], ..., init (init xs), init xs, xs]`
  -- - If `xs` is nonempty, it follows that `inits xs` contains at least one nonempty
  --   list, since `last (inits xs) = xs`.
  -- - The only empty element of `inits xs` is the first one (by the definition of `inits`)
  -- - Therefore, if we take all but the first element of `inits xs` i.e.
  --   `tail (inits xs)`, we have a nonempty list of nonempty lists
  fromList . Prelude.map fromList . List.tail . List.inits . Foldable.toList

-- | The 'tails1' function takes a 'NonEmpty' stream @xs@ and returns all the
-- non-empty suffixes of @xs@, starting with the longest.
--
-- > tails1 (1 :| [2,3]) == (1 :| [2,3]) :| [2 :| [3], 3 :| []]
-- > tails1 (1 :| []) == (1 :| []) :| []
--
-- /Since: 4.18/
tails1 :: NonEmpty a -> NonEmpty (NonEmpty a)
tails1 =
  -- fromList is an unsafe function, but this usage should be safe, since:
  -- - `tails xs = [xs, tail xs, tail (tail xs), ..., []]`
  -- - If `xs` is nonempty, it follows that `tails xs` contains at least one nonempty
  --   list, since `head (tails xs) = xs`.
  -- - The only empty element of `tails xs` is the last one (by the definition of `tails`)
  -- - Therefore, if we take all but the last element of `tails xs` i.e.
  --   `init (tails xs)`, we have a nonempty list of nonempty lists
  fromList . Prelude.map fromList . List.init . List.tails . Foldable.toList
#endif
