{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}
#endif
module Data.Tuple.Compat
  (
#if MIN_VERSION_ghc_prim(0,10,0)
    Solo(MkSolo, Solo)
#elif __GLASGOW_HASKELL__ >= 708 && \
      __GLASGOW_HASKELL__ < 800 && \
      defined(MIN_VERSION_OneTuple)
# if MIN_VERSION_OneTuple(0,4,0)
    Solo
  , pattern Solo
# else
    Solo(Solo)
# endif
#else
    Solo(Solo)
#endif
  , fst
  , snd
  , curry
  , uncurry
  , swap
  ) where

#if MIN_VERSION_ghc_prim(0,7,0)
import "base-compat" Data.Tuple.Compat
#else
import "base" Data.Tuple
import "OneTuple" Data.Tuple.Solo
#endif
