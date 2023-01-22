{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if !(MIN_VERSION_base(4,18,0))
{-# LANGUAGE ScopedTypeVariables #-}
#endif
module Data.Traversable.Compat (
  module Base
, mapAccumM
, forAccumM
) where

import Data.Traversable as Base

#if !(MIN_VERSION_base(4,18,0))
import Prelude.Compat

import Control.Monad.Compat (liftM)

# if MIN_VERSION_base(4,8,0)
import Data.Coerce (Coercible, coerce)
# else
import Unsafe.Coerce (unsafeCoerce)
# endif
#endif

#if !(MIN_VERSION_base(4,18,0))
-- | A state transformer monad parameterized by the state and inner monad.
-- The implementation is copied from the transformers package with the
-- return tuple swapped.
--
-- /Since: 4.18.0.0/
newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

-- | /Since: 4.18.0.0/
instance Monad m => Functor (StateT s m) where
    fmap = liftM
    {-# INLINE fmap #-}

-- | /Since: 4.18.0.0/
instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (s, a)
    {-# INLINE pure #-}
    StateT mf <*> StateT mx = StateT $ \ s -> do
        (s', f) <- mf s
        (s'', x) <- mx s'
        return (s'', f x)
    {-# INLINE (<*>) #-}
    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}

# if MIN_VERSION_base(4,8,0)
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
# else
(#.) ::                  (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = unsafeCoerce
# endif

-- | /Since: 4.18.0.0/
instance (Monad m) => Monad (StateT s m) where
    m >>= k  = StateT $ \ s -> do
        (s', a) <- runStateT m s
        runStateT (k a) s'
    {-# INLINE (>>=) #-}
# if !(MIN_VERSION_base(4,11,0))
    return = pure
# endif

-- | The `mapAccumM` function behaves like a combination of `mapM` and
-- `mapAccumL` that traverses the structure while evaluating the actions
-- and passing an accumulating parameter from left to right.
-- It returns a final value of this accumulator together with the new structure.
-- The accummulator is often used for caching the intermediate results of a computation.
--
--  @since 4.18.0.0
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> let expensiveDouble a = putStrLn ("Doubling " <> show a) >> pure (2 * a)
-- >>> :{
-- mapAccumM (\cache a -> case lookup a cache of
--     Nothing -> expensiveDouble a >>= \double -> pure ((a, double):cache, double)
--     Just double -> pure (cache, double)
--     ) [] [1, 2, 3, 1, 2, 3]
-- :}
-- Doubling 1
-- Doubling 2
-- Doubling 3
-- ([(3,6),(2,4),(1,2)],[2,4,6,2,4,6])
--
mapAccumM
  :: forall m t s a b. (Monad m, Traversable t)
  => (s -> a -> m (s, b))
  -> s -> t a -> m (s, t b)
mapAccumM f s t = runStateT (mapM (StateT #. flip f) t) s

-- | 'forAccumM' is 'mapAccumM' with the arguments rearranged.
--
-- @since 4.18.0.0
forAccumM
  :: (Monad m, Traversable t)
  => s -> t a -> (s -> a -> m (s, b)) -> m (s, t b)
{-# INLINE forAccumM #-}
forAccumM s t f = mapAccumM f s t
#endif
