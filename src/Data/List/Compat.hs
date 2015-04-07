{-# LANGUAGE BangPatterns #-}
module Data.List.Compat (
  module Base
#if !MIN_VERSION_base(4,8,0)
, all
, and
, any
, concat
, concatMap
, elem
, find
, foldl
, foldl'
, foldl1
, foldr
, foldr1
, length
, maximum
, maximumBy
, minimum
, minimumBy
, notElem
, null
, or
, product
, sum
, mapAccumL
, mapAccumR

, isSubsequenceOf
, sortOn
, uncons
, scanl'
#endif

#if !MIN_VERSION_base(4,5,0)
, dropWhileEnd
#endif
) where

#if MIN_VERSION_base(4,8,0)
import Data.List as Base
#else
import Data.List as Base hiding (
    all
  , and
  , any
  , concat
  , concatMap
  , elem
  , find
  , foldl
  , foldl'
  , foldl1
  , foldr
  , foldr1
  , length
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  , notElem
  , null
  , or
  , product
  , sum
  , mapAccumL
  , mapAccumR
  )
import Data.Foldable.Compat
import Data.Traversable.Compat
import Prelude.Compat hiding (foldr, null)
import Data.Ord (comparing)
#endif

#if !MIN_VERSION_base(4,5,0)
-- | The 'dropWhileEnd' function drops the largest suffix of a list
-- in which the given predicate holds for all elements.  For example:
--
-- > dropWhileEnd isSpace "foo\n" == "foo"
-- > dropWhileEnd isSpace "foo bar" == "foo bar"
-- > dropWhileEnd isSpace ("foo\n" ++ undefined) == "foo" ++ undefined
--
-- /Since: 4.5.0.0/
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

#endif

#if !MIN_VERSION_base(4,8,0)
-- | The 'isSubsequenceOf' function takes two lists and returns 'True' if the
-- first list is a subsequence of the second list.
--
-- @'isSubsequenceOf' x y@ is equivalent to @'elem' x ('subsequences' y)@.
--
-- /Since: 4.8.0.0/
--
-- ==== __Examples__
--
-- >>> isSubsequenceOf "GHC" "The Glorious Haskell Compiler"
-- True
-- >>> isSubsequenceOf ['a','d'..'z'] ['a'..'z']
-- True
-- >>> isSubsequenceOf [1..10] [10,9..0]
-- False
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []    _                    = True
isSubsequenceOf _     []                   = False
isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
                               | otherwise = isSubsequenceOf a b

-- | Sort a list by comparing the results of a key function applied to each
-- element.  @sortOn f@ is equivalent to @sortBy . comparing f@, but has the
-- performance advantage of only evaluating @f@ once for each element in the
-- input list.  This is called the decorate-sort-undecorate paradigm, or
-- Schwartzian transform.
--
-- /Since: 4.8.0.0/
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

-- | Decompose a list into its head and tail. If the list is empty,
-- returns 'Nothing'. If the list is non-empty, returns @'Just' (x, xs)@,
-- where @x@ is the head of the list and @xs@ its tail.
--
-- /Since: 4.8.0.0/
uncons                  :: [a] -> Maybe (a, [a])
uncons []               = Nothing
uncons (x:xs)           = Just (x, xs)

-- | A strictly accumulating version of 'scanl'
{-# NOINLINE [1] scanl' #-}
scanl'           :: (b -> a -> b) -> b -> [a] -> [b]
-- This peculiar form is needed to prevent scanl' from being rewritten
-- in its own right hand side.
scanl' = scanlGo'
  where
    scanlGo'           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo' f !q ls    = q : (case ls of
                            []   -> []
                            x:xs -> scanlGo' f (f q x) xs)
#endif
