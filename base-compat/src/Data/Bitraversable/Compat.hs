{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Bitraversable.Compat (
#if MIN_VERSION_base(4,10,0)
  module Base
, firstA
, secondA
#endif
) where

#if MIN_VERSION_base(4,10,0)
import Data.Bitraversable as Base

# if !MIN_VERSION_base(4,21,0)
import Prelude.Compat
# endif

# if !MIN_VERSION_base(4,21,0)
-- | Traverses only over the first argument.
--
-- @'firstA' f ≡ 'bitraverse' f 'pure'@

-- ==== __Examples__
--
-- Basic usage:
--
-- >>> firstA listToMaybe (Left [])
-- Nothing
--
-- >>> firstA listToMaybe (Left [1, 2, 3])
-- Just (Left 1)
--
-- >>> firstA listToMaybe (Right [4, 5])
-- Just (Right [4, 5])
--
-- >>> firstA listToMaybe ([1, 2, 3], [4, 5])
-- Just (1,[4, 5])
--
-- >>> firstA listToMaybe ([], [4, 5])
-- Nothing

-- @since 4.21.0.0
firstA :: Bitraversable t => Applicative f => (a -> f c) -> t a b -> f (t c b)
firstA f = bitraverse f pure

-- | Traverses only over the second argument.
--
-- @'secondA' f ≡ 'bitraverse' 'pure' f@
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> secondA (find odd) (Left [])
-- Just (Left [])
--
-- >>> secondA (find odd) (Left [1, 2, 3])
-- Just (Left [1,2,3])
--
-- >>> secondA (find odd) (Right [4, 5])
-- Just (Right 5)
--
-- >>> secondA (find odd) ([1, 2, 3], [4, 5])
-- Just ([1,2,3],5)
--
-- >>> secondA (find odd) ([1,2,3], [4])
-- Nothing
--
-- @since 4.21.0.0
secondA :: Bitraversable t => Applicative f => (b -> f c) -> t a b -> f (t a c)
secondA f = bitraverse pure f
# endif
#endif
