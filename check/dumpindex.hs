module Main (main) where

import           Data.List
import           System.Environment

import           Util

main :: IO ()
main = do
  [module_] <- getArgs
  index module_

index :: String -> IO ()
index module_ = ghci ["check-hs/" ++ module_ ++ ".check.hs"] input >>= putStr . normalize
  where
    input = ":browse! " ++ module_

normalize :: String -> String
normalize = unlines . map removeSignature . removeTypeDefinitions . lines . normalizeSignatures . unlines . removeClasses . normalizeClasses . lines

removeTypeDefinitions :: [String] -> [String]
removeTypeDefinitions = filter (not . isPrefixOf "type ")

removeClasses :: [String] -> [String]
removeClasses = removeOmissionDots . go
  where
    removeOmissionDots = filter (/= "...")

    go input = case input of
      x : xs | "class " `isPrefixOf` x -> case span ("  " `isPrefixOf`) xs of
        (ys, zs) -> map (drop 2) ys ++ go zs
      x : xs -> x : go xs
      [] -> []

normalizeClasses :: [String] -> [String]
normalizeClasses input = case input of
  x : y : ys | "class " `isPrefixOf` x && "      " `isPrefixOf` y -> normalizeClasses ((x ++ y) : ys)
  y : ys -> y : normalizeClasses ys
  [] -> []

removeSignature :: String -> String
removeSignature = go
  where
    go input = case input of
      x : xs | " :: " `isPrefixOf` xs -> [x]
      x : xs -> x : go xs
      [] -> []
