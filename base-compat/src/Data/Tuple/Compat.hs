{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if MIN_VERSION_ghc_prim(0,7,0)
{-# LANGUAGE PatternSynonyms #-}
#endif
-- | Note that we only re-export @MkSolo@ when building with @ghc-prim-0.10.0@
-- (bundled with GHC 9.6) or later. If you want to backport @MkSolo@ to older
-- versions of GHC, import @Data.Tuple.Compat@ from @base-compat-batteries@
-- instead.
module Data.Tuple.Compat
  ( fst
  , snd
  , curry
  , uncurry
  , swap
#if MIN_VERSION_ghc_prim(0,10,0)
  , Solo(MkSolo,Solo)
#elif MIN_VERSION_ghc_prim(0,7,0)
  , Solo(Solo)
#endif
#if MIN_VERSION_ghc_prim(0,8,0)
  , getSolo
#endif
  ) where

import Data.Tuple

#if !(MIN_VERSION_base(4,16,0)) && MIN_VERSION_ghc_prim(0,7,0)
import GHC.Tuple (Solo(..))
#endif

#if MIN_VERSION_ghc_prim(0,8,0) && !(MIN_VERSION_ghc_prim(0,11,0))
import GHC.Tuple (getSolo)
#endif
