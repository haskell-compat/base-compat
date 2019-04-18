{-# LANGUAGE CPP #-}
module Main (main) where

import           Control.Monad (unless)
import           Data.List ((\\))

import           System.Directory
import           System.Exit
import           System.FilePath (dropExtension)
import           System.IO
import           System.IO.Temp (withTempFile)
import           System.Process

import           Test.Hspec (Spec, describe, hspec, it)
import           Test.HUnit (assertFailure)

import           TypeDump (typeDump)
import System.Environment (getEnv)

main :: IO ()
main = do
  getEnv "PATH" >>= print

  pwd  <- getCurrentDirectory
  mods <- modules
  hspec $ mapM_ (typeCheck pwd) mods

modules :: IO [FilePath]
modules = do
  contents <- getDirectoryContents "index"
  let noNavigation = contents \\ [".", ".."]
      noFileExts   = map dropExtension noNavigation
      noExcluded   = noFileExts \\ excluded
  return noExcluded
    where
      excluded :: [FilePath]
      excluded = [
#if !(MIN_VERSION_base(4,4,0))
          "Data.Complex.Compat"
        , "Data.Ratio.Compat"
#endif
        ]

typeCheck :: FilePath -> String -> Spec
typeCheck pwd module_ =
    describe module_
  . it "should have the expected type signatures"
  . withTempFile pwd "Temp.types" $ \fp h -> do
      types <- typeDump module_
      hPutStr h types
      hFlush h

      (exitCode, stdOut, stdErr) <-
        readProcessWithExitCode "typediff" ["types/" ++ module_ ++ ".types", fp] ""

      case exitCode of
        ExitFailure i ->
          assertFailure $ unlines [ "typediff failed with code: " ++ show i
                                  , stdErr
                                  ]
        ExitSuccess -> unless (null stdOut) $ assertFailure stdOut
