{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
module Data.Tuple.Compat
  ( fst
  , snd
  , curry
  , uncurry
  , swap
#if MIN_VERSION_ghc_prim(0,7,0)
  , Solo(..)
#endif
  ) where

import Data.Tuple

#if MIN_VERSION_ghc_prim(0,7,0)
import GHC.Tuple (Solo(..))
#endif
