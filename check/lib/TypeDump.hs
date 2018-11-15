module TypeDump (typeDump) where

import Data.List (isPrefixOf)
import           Util

typeDump :: String -> IO String
typeDump module_ = readFile ("index/" ++ module_ ++ ".index") >>= types module_ . lines

types :: String -> [String] -> IO String
types module_ names = do
    output' <- inCabalNewRepl ("check-hs/" ++ module_ ++ ".check.hs") input
    let output = normalizeSignatures output'
    putStrLn "=== NORMALISED ==="
    putStrLn output
    return output
  where
    input = unlines $ map typeOf_ names

    typeOf_ line@('-':'-':_) = line
    typeOf_ line
        | "data "    `isPrefixOf` line = "-- skipping: " ++ line
        | "newtype " `isPrefixOf` line = "-- skipping: " ++ line
    typeOf_ line = ":t " ++ line
