{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Trustworthy #-}
module Data.List.Compat (
  module Base

#if !(MIN_VERSION_base(4,21,0))
, inits1
, tails1
#endif

#if MIN_VERSION_base(4,18,0) && !(MIN_VERSION_base(4,20,0))
, List
#endif

#if !(MIN_VERSION_base(4,19,0))
, (!?)
, unsnoc
#endif

#if !(MIN_VERSION_base(4,15,0))
, singleton
#endif

#if !(MIN_VERSION_base(4,11,0))
, iterate'
#endif
) where

import Data.List as Base

#if !(MIN_VERSION_base(4,11,0))
import GHC.Exts (build)
#endif

#if !(MIN_VERSION_base(4,19,0))
import Prelude.Compat hiding (foldr, null)
#endif

#if MIN_VERSION_base(4,18,0) && !(MIN_VERSION_base(4,20,0))
import GHC.List (List)
#endif

#if !(MIN_VERSION_base(4,21,0))
import Data.List.NonEmpty (NonEmpty(..))
import GHC.List (build)
#endif

#if !(MIN_VERSION_base(4,11,0))
-- | 'iterate\'' is the strict version of 'iterate'.
--
-- It ensures that the result of each application of force to weak head normal
-- form before proceeding.
{-# NOINLINE [1] iterate' #-}
iterate' :: (a -> a) -> a -> [a]
iterate' f x =
    let x' = f x
    in x' `seq` (x : iterate' f x')

{-# INLINE [0] iterate'FB #-} -- See Note [Inline FB functions]
iterate'FB :: (a -> b -> b) -> (a -> a) -> a -> b
iterate'FB c f x0 = go x0
  where go x =
            let x' = f x
            in x' `seq` (x `c` go x')

{-# RULES
"iterate'"    [~1] forall f x.   iterate' f x = build (\c _n -> iterate'FB c f x)
"iterate'FB"  [1]                iterate'FB (:) = iterate'
 #-}
#endif

#if !(MIN_VERSION_base(4,15,0))
-- | Produce singleton list.
--
-- >>> singleton True
-- [True]
--
-- /Since: 4.14.0.0/
--
singleton :: a -> [a]
singleton x = [x]
#endif

#if !(MIN_VERSION_base(4,19,0))
infixl 9 !?
-- | List index (subscript) operator, starting from 0. Returns 'Nothing'
-- if the index is out of bounds
--
-- >>> ['a', 'b', 'c'] !? 0
-- Just 'a'
-- >>> ['a', 'b', 'c'] !? 2
-- Just 'c'
-- >>> ['a', 'b', 'c'] !? 3
-- Nothing
-- >>> ['a', 'b', 'c'] !? (-1)
-- Nothing
--
-- This is the total variant of the partial '!!' operator.
--
-- WARNING: This function takes linear time in the index.
(!?) :: [a] -> Int -> Maybe a

{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n

-- | \(\mathcal{O}(n)\). Decompose a list into 'init' and 'last'.
--
-- * If the list is empty, returns 'Nothing'.
-- * If the list is non-empty, returns @'Just' (xs, x)@,
-- where @xs@ is the 'init'ial part of the list and @x@ is its 'last' element.
--
-- /Since: 4.19.0.0/
--
-- >>> unsnoc []
-- Nothing
-- >>> unsnoc [1]
-- Just ([],1)
-- >>> unsnoc [1, 2, 3]
-- Just ([1,2],3)
--
-- Laziness:
--
-- >>> fst <$> unsnoc [undefined]
-- Just []
-- >>> head . fst <$> unsnoc (1 : undefined)
-- Just *** Exception: Prelude.undefined
-- >>> head . fst <$> unsnoc (1 : 2 : undefined)
-- Just 1
--
-- 'unsnoc' is dual to 'uncons': for a finite list @xs@
--
-- > unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)
--
unsnoc :: [a] -> Maybe ([a], a)
-- The lazy pattern ~(a, b) is important to be productive on infinite lists
-- and not to be prone to stack overflows.
-- Expressing the recursion via 'foldr' provides for list fusion.
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
{-# INLINABLE unsnoc #-}
#endif

#if !(MIN_VERSION_base(4,21,0))
inits1, tails1 :: [a] -> [NonEmpty a]

-- | The 'inits1' function returns all non-empty initial segments of the
-- argument, shortest first.
--
-- @since 4.21.0.0
--
-- ==== __Laziness__
--
-- Note that 'inits1' has the following strictness property:
-- @inits1 (xs ++ _|_) = inits1 xs ++ _|_@
--
-- In particular,
-- @inits1 _|_ = _|_@
--
-- ==== __Examples__
--
-- >>> inits1 "abc"
-- ['a' :| "",'a' :| "b",'a' :| "bc"]
--
-- >>> inits1 []
-- []
--
-- inits1 is productive on infinite lists:
--
-- >>> take 3 $ inits1 [1..]
-- [1 :| [],1 :| [2],1 :| [2,3]]
inits1 [] = []
inits1 (x : xs) = map (x :|) (inits xs)

-- | \(\mathcal{O}(n)\). The 'tails1' function returns all non-empty final
-- segments of the argument, longest first.
--
-- @since 4.21.0.0
--
-- ==== __Laziness__
--
-- Note that 'tails1' has the following strictness property:
-- @tails1 _|_ = _|_@
--
-- >>> tails1 undefined
-- *** Exception: Prelude.undefined
--
-- >>> drop 1 (tails1 [undefined, 1, 2])
-- [1 :| [2],2 :| []]
--
-- ==== __Examples__
--
-- >>> tails1 "abc"
-- ['a' :| "bc",'b' :| "c",'c' :| ""]
--
-- >>> tails1 [1, 2, 3]
-- [1 :| [2,3],2 :| [3],3 :| []]
--
-- >>> tails1 []
-- []
{-# INLINABLE tails1 #-}
tails1 lst = build (\c n ->
  let tails1Go [] = n
      tails1Go (x : xs) = (x :| xs) `c` tails1Go xs
  in tails1Go lst)
#endif
