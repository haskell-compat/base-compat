module Main (main) where

import           System.Environment
import           System.Process

main :: IO ()
main = do
  [module_] <- getArgs
  readFile ("check/" ++ module_ ++ ".index") >>= signatures module_ . lines

signatures :: String -> [String] -> IO ()
signatures module_ names =
  readProcess "ghci" ["-v0", "-fno-warn-duplicate-exports", "-isrc", "-XNoImplicitPrelude", "-ignore-dot-ghci", "-XCPP", "-optP-include", "-optPdist/build/autogen/cabal_macros.h", "check/" ++ module_ ++ ".check.hs"] input >>= putStr . normalize
  where
    input = unlines $ map (":t " ++) names

normalize :: String -> String
normalize xs = case xs of
  '\n':' ':' ':ys -> ' ' : normalize ys
  y:ys -> y : normalize ys
  "" -> ""
