module Main (main) where

import           System.Process
import           Data.Char

main :: IO ()
main = readFile "Prelude.index" >>= signatures . lines

signatures :: [String] -> IO ()
signatures names = readProcess "ghci" ["-v0", "-isrc", "-XNoImplicitPrelude", "-ignore-dot-ghci", "-XCPP", "-optP-include", "-optPdist/build/autogen/cabal_macros.h", "PreludeTest.hs"] input >>= putStr . normalize
  where
    input = unlines $ map (":t " ++) names

normalize :: String -> String
normalize xs = case xs of
  '\n':' ':' ':ys -> ' ' : normalize ys
  y:ys -> y : normalize ys
  "" -> ""
