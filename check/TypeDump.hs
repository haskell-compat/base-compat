module TypeDump (typeDump) where

import Util

typeDump :: String -> IO String
typeDump module_ = readFile ("index/" ++ module_ ++ ".index") >>= types module_ . lines

types :: String -> [String] -> IO String
types module_ names = fmap normalizeSignatures $ ghci ["check-hs/" ++ module_ ++ ".check.hs"] input
  where
    input = unlines $ map (":t " ++) names
