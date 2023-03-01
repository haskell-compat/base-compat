{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}
#endif
-- | This uses the @OneTuple@ compatibility library to backport 'Solo' to old
-- versions of GHC. Note that @OneTuple@ makes use of pattern synonyms, which
-- cannot be defined on pre-7.8 versions of GHC. As such, it is not feasible
-- to backport the @Solo@ data constructor on pre-7.8 versions of GHC, as
-- @OneTuple@ defines this as a pattern synonym.
module Data.Tuple.Compat
  (
#if MIN_VERSION_ghc_prim(0,10,0)
    Solo(MkSolo, Solo)
#elif MIN_VERSION_ghc_prim(0,7,0)
    Solo(Solo)
  , pattern MkSolo
#elif __GLASGOW_HASKELL__ >= 800
    Solo(MkSolo, Solo)
#elif __GLASGOW_HASKELL__ >= 708
    Solo(MkSolo)
  , pattern Solo
#else
    Solo(MkSolo)
#endif
  , fst
  , snd
  , curry
  , uncurry
  , swap
  ) where

#if MIN_VERSION_ghc_prim(0,10,0)
import "base-compat" Data.Tuple.Compat
#elif MIN_VERSION_ghc_prim(0,7,0)
import "base-compat" Data.Tuple.Compat
import "OneTuple" Data.Tuple.Solo (pattern MkSolo)
#else
import "base" Data.Tuple
import "OneTuple" Data.Tuple.Solo
#endif
