{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
-- | This uses the @OneTuple@ compatibility library to backport 'Solo' to old
-- versions of GHC.
module Data.Tuple.Compat
  (
#if MIN_VERSION_ghc_prim(0,10,0)
    Solo(MkSolo, Solo)
#elif MIN_VERSION_ghc_prim(0,7,0)
    Solo(Solo)
  , pattern MkSolo
#else
    Solo(MkSolo, Solo)
#endif
  , getSolo
  , fst
  , snd
  , curry
  , uncurry
  , swap
  ) where

#if MIN_VERSION_ghc_prim(0,10,0)
import "base-compat" Data.Tuple.Compat
#elif MIN_VERSION_ghc_prim(0,8,0)
import "base-compat" Data.Tuple.Compat
import "OneTuple" Data.Tuple.Solo (pattern MkSolo)
#elif MIN_VERSION_ghc_prim(0,7,0)
import "base-compat" Data.Tuple.Compat
import "OneTuple" Data.Tuple.Solo (pattern MkSolo, getSolo)
#else
import "base" Data.Tuple
import "OneTuple" Data.Tuple.Solo
#endif
