module Main (main) where

import Control.Monad (when)
import Data.List
import System.Directory
import System.FilePath

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  rootDir <- findRootDir cwd
  putStrLn $ "Root dir: " ++ rootDir
  mkReplModules (rootDir </> "base-compat")           ["Repl"]
  mkReplModules (rootDir </> "base-compat-batteries") ["Repl", "Batteries"]

-- Go up until there is 'cabal.project'
findRootDir :: FilePath -> IO FilePath
findRootDir fp
  | isDrive fp = return fp
  | otherwise = do
    exists <- doesFileExist $ fp </> "cabal.project"
    if exists
    then return fp
    else findRootDir (takeDirectory fp)

mkReplModules :: FilePath -> [String] -> IO ()
mkReplModules dir suffixes = do
  preprocessPathRecursive mkReplModule (dir </> "src")
  where
    mkReplModule :: FilePath -> IO ()
    mkReplModule entry =
      when (takeFileName entry == "Compat.hs") $ do
        let parentDir      = takeDirectory entry
            sepStr         = [pathSeparator]
            Just modPrefix = stripPrefix (dir </> "src" ++ sepStr) parentDir
            modName        = replace sepStr "." modPrefix <.> "Compat"
            replFileName   = parentDir </> "Compat"
                                     </> intercalate sepStr suffixes
                                     <.> "hs"
            replFileDir  = takeDirectory replFileName
        createDirectoryIfMissing True replFileDir
        writeFile replFileName $ fileTemplate modName suffixes

fileTemplate :: String -> [String] -> String
fileTemplate modName suffixes = unlines
  [ "{-# LANGUAGE PackageImports #-}"
  , "{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}"
  , "-- | Reexports \"" ++ modName ++ "\""
  , "-- from a globally unique namespace."
  , "module " ++ modName ++ "." ++ intercalate "." suffixes ++ " ("
  , "  module " ++ modName
  , ") where"
  , "import \"this\" " ++ modName
  ]

-- | Replace a subsequence everywhere it occurs. The first argument must
--   not be the empty list.
--
-- Taken from the @extra@ library.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = error "replace: first argument cannot be empty"
replace from to xs | Just xs <- stripPrefix from xs = to ++ replace from to xs
replace from to (x:xs) = x : replace from to xs
replace from to [] = []

-- | Traverse the directory tree in preorder.
--
-- Taken from the @directory@ test suite.
preprocessPathRecursive :: (FilePath -> IO ()) -> FilePath -> IO ()
preprocessPathRecursive f path = do
  dirExists <- doesDirectoryExist path
  if dirExists
    then do
      f path
      names <- listDirectory path
      mapM_ (preprocessPathRecursive f) (fmap (path </>) names)
    else f path
