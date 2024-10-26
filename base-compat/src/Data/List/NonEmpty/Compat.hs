{-# LANGUAGE CPP, NoImplicitPrelude #-}
-- | This backports the modern "Data.Semigroup" interface back to
-- @base-4.9@/GHC 8.0.
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
  , sortOn
  , reverse
  , inits
  , inits1
  , tails
  , tails1
  , append
  , appendList
  , prependList
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
  , permutations
  , permutations1
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

import Data.List.NonEmpty

#if !(MIN_VERSION_base(4,20,0))
import qualified Prelude.Compat as Prelude
import Prelude.Compat ((.))

import qualified Data.Foldable.Compat as Foldable
import qualified Data.List.Compat as List
#endif

#if !(MIN_VERSION_base(4,15,0))
-- | Construct a 'NonEmpty' list from a single element.
--
-- /Since: 4.15/
singleton :: a -> NonEmpty a
singleton a = a :| []
#endif

#if !(MIN_VERSION_base(4,16,0))
-- | A monomorphic version of 'Prelude.<>' for 'NonEmpty'.
--
-- >>> append (1 :| []) (2 :| [3])
-- 1 :| [2,3]
--
-- /Since: 4.16/
append :: NonEmpty a -> NonEmpty a -> NonEmpty a
append = (Prelude.<>)

-- | Attach a list at the end of a 'NonEmpty'.
--
-- >>> appendList (1 :| [2,3]) []
-- 1 :| [2,3]
--
-- >>> appendList (1 :| [2,3]) [4,5]
-- 1 :| [2,3,4,5]
--
-- /Since: 4.16/
appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (x :| xs) ys = x :| xs Prelude.<> ys

-- | Attach a list at the beginning of a 'NonEmpty'.
--
-- >>> prependList [] (1 :| [2,3])
-- 1 :| [2,3]
--
-- >>> prependList [negate 1, 0] (1 :| [2, 3])
-- -1 :| [0,1,2,3]
--
-- /Since: 4.16/
prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList ls ne = case ls of
  [] -> ne
  (x : xs) -> x :| xs Prelude.<> toList ne
#endif

#if !(MIN_VERSION_base(4,18,0))
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

#if !(MIN_VERSION_base(4,20,0))
-- | The 'permutations' function returns the list of all permutations of the argument.
--
-- /Since: 4.20.0.0/
permutations            :: [a] -> NonEmpty [a]
permutations xs0        =  xs0 :| perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = List.foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' Prelude.id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)
-- The implementation of 'permutations' is adopted from 'GHC.Internal.Data.List.permutations',
-- see there for discussion and explanations.

-- | 'permutations1' operates like 'permutations', but uses the knowledge that its input is
-- non-empty to produce output where every element is non-empty.
--
-- > permutations1 = fmap fromList . permutations . toList
--
-- /Since: 4.20.0.0/
permutations1 :: NonEmpty a -> NonEmpty (NonEmpty a)
permutations1 xs = fromList Prelude.<$> permutations (toList xs)

-- | Sort a 'NonEmpty' on a user-supplied projection of its elements.
-- See 'List.sortOn' for more detailed information.
--
-- ==== __Examples__
--
-- >>> sortOn fst $ (2, "world") :| [(4, "!"), (1, "Hello")]
-- (1,"Hello") :| [(2,"world"),(4,"!")]
--
-- >>> sortOn length $ "jim" :| ["creed", "pam", "michael", "dwight", "kevin"]
-- "jim" :| ["pam","creed","kevin","dwight","michael"]
--
-- ==== __Performance notes__
--
-- This function minimises the projections performed, by materialising
-- the projections in an intermediate list.
--
-- For trivial projections, you should prefer using 'sortBy' with
-- 'comparing', for example:
--
-- >>> sortBy (comparing fst) $ (3, 1) :| [(2, 2), (1, 3)]
-- (1,3) :| [(2,2),(3,1)]
--
-- Or, for the exact same API as 'sortOn', you can use `sortBy . comparing`:
--
-- >>> (sortBy . comparing) fst $ (3, 1) :| [(2, 2), (1, 3)]
-- (1,3) :| [(2,2),(3,1)]
--
-- 'sortWith' is an alias for `sortBy . comparing`.
--
-- /Since: 4.20.0.0/
sortOn :: Prelude.Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
sortOn f = lift (List.sortOn f)

-- | Lift list operations to work on a 'NonEmpty' stream.
--
-- /Beware/: If the provided function returns an empty list,
-- this will raise an error.
lift :: Foldable.Foldable f => ([a] -> [b]) -> f a -> NonEmpty b
lift f = fromList . f . Foldable.toList
#endif
