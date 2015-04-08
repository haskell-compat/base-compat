module Main (main) where

import           System.Environment

import           Util

main :: IO ()
main = do
  [module_] <- getArgs
  readFile ("check/" ++ module_ ++ ".index") >>= types module_ . lines

types :: String -> [String] -> IO ()
types module_ names = ghci ["check/" ++ module_ ++ ".check.hs"] input >>= putStr . normalizeSignatures
  where
    input = unlines $ map (":t " ++) names
