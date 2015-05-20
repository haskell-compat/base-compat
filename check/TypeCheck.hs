module Main (main) where

import Control.Monad (unless)

import System.Directory (getCurrentDirectory)
import System.Exit
import System.IO
import System.IO.Temp (withTempFile)
import System.Process

import Test.Hspec (Spec, describe, hspec, it)
import Test.HUnit (assertFailure)

import TypeDump (typeDump)

main :: IO ()
main = do
    pwd <- getCurrentDirectory
    hspec $ mapM_ (typeCheck pwd) modules

modules :: [String]
modules = [ "Prelude.Compat"
          , "Control.Monad.Compat"
          , "Data.Foldable.Compat"
          , "Data.List.Compat"
          ]

typeCheck :: FilePath -> String -> Spec
typeCheck pwd module_ = describe module_
                  . it "should have the expected type signatures"
                  . withTempFile pwd "Temp.types" $ \fp h -> do
    types <- typeDump module_
    hPutStr h types
    hFlush h

    let typeDiff = "typediff types/" ++ module_ ++ ".types " ++ fp
    (_,mbStdoutH,_,rTypediff) <- createProcess $ (shell typeDiff) {
        std_out = CreatePipe
    }
    exitCode <- waitForProcess rTypediff

    case exitCode of
         ExitFailure i -> assertFailure $ "typediff failed with code: " ++ show i
         ExitSuccess   ->
            maybe (assertFailure "no stdout")
                  (\hTypediff -> do
                      output <- hGetContents hTypediff
                      unless (null output) $ assertFailure output
                  )
                  mbStdoutH
