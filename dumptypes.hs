module Main (main) where

import           System.Process
import           Data.Char

main :: IO ()
main = readFile "Prelude.index" >>= signatures . lines

signatures :: [String] -> IO ()
signatures names = readProcess "ghci" ["-v0", "-XNoImplicitPrelude", "-ignore-dot-ghci", "-XCPP", "-optP-include", "-optPdist/build/autogen/cabal_macros.h", "src/Prelude/Compat.hs"] input >>= putStr . normalize
  where
    input = unlines $ ":m - Prelude.Compat" : ":m + Prelude.Compat" : map (":t " ++) names

normalize :: String -> String
normalize xs = case xs of
  '\n':' ':' ':ys -> ' ' : normalize ys
  y:ys -> y : normalize ys
  "" -> ""
