module Main (main) where

import System.Environment (getArgs)
import TypeDump (typeDump)

main :: IO ()
main = do
  [module_] <- getArgs
  types <- typeDump module_
  putStr types
