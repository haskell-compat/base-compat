{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Tuple.Compat
  ( Solo (..)
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
import "OneTuple" Data.Tuple.Solo (Solo(..))
#endif
