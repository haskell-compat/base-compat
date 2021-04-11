module Main (main) where

import qualified Data.List as List
import           System.Environment

import           Util

main :: IO ()
main = do
  [module_] <- getArgs
  index module_

index :: String -> IO ()
index module_ = inCabalNewRepl ("check-hs/" ++ module_ ++ ".check.hs") input >>= putStr . normalize
  where
    input = ":browse! " ++ module_

normalize :: String -> String
normalize = unlines . map removeSignature . removeTypeDefinitions . lines . normalizeLines . unlines . removeClasses . normalizeClasses . lines

removeTypeDefinitions :: [String] -> [String]
removeTypeDefinitions = filter (not . List.isPrefixOf "type ")

removeClasses :: [String] -> [String]
removeClasses = removeOmissionDots . go
  where
    removeOmissionDots = filter (/= "...")

    go input = case input of
      x : xs | "class " `List.isPrefixOf` x -> case span ("  " `List.isPrefixOf`) xs of
        (ys, zs) -> map (drop 2) ys ++ go zs
      x : xs -> x : go xs
      [] -> []

normalizeClasses :: [String] -> [String]
normalizeClasses input = case input of
  x : y : ys | "class " `List.isPrefixOf` x && "      " `List.isPrefixOf` y -> normalizeClasses ((x ++ y) : ys)
  y : ys -> y : normalizeClasses ys
  [] -> []

removeSignature :: String -> String
removeSignature = go
  where
    go input = case input of
      x : xs | " :: " `List.isPrefixOf` xs -> [x]
      x : xs -> x : go xs
      [] -> []
