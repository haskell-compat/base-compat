module Main where

import           Control.Monad
import           System.Environment
import           System.Exit
import           System.IO

import           TypeDiff

main :: IO ()
main = do
  [file1, file2] <- getArgs
  input1 <- readFile_ file1
  input2 <- readFile_ file2
  let diff = typeDiff input1 input2
  unless (null diff) $ do
    hPutStr stderr diff
    exitFailure

readFile_ :: FilePath -> IO String
readFile_ name = case name of
  "-" -> getContents
  _ -> readFile name
