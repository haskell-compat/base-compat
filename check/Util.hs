module Util where

import System.Process

ghci :: [String] -> String -> IO String
ghci args_ input = readProcess "ghci" args input
  where
    args = ["-v0", "-fno-warn-duplicate-exports", "-isrc", "-XNoImplicitPrelude", "-ignore-dot-ghci", "-optP-include", "-optPdist/build/autogen/cabal_macros.h"] ++ args_

normalizeSignatures :: String -> String
normalizeSignatures input = case input of
  '\n':' ':' ':ys -> ' ' : normalizeSignatures ys
  y:ys -> y : normalizeSignatures ys
  "" -> ""
