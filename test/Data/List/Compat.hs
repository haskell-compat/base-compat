module Data.List.CompatSpec (main, spec) where

import           Test.Hspec
import           Data.List.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "dropWhileEnd" $ do
    it "drops the largest suffix of a list in which a predicate holds for all elements" $ do
      dropWhileEnd (== ' ') "foo "    `shouldBe` "foo"
      dropWhileEnd (== ' ') "foo bar" `shouldBe` "foo bar"
  describe "isSubsequenceOf" $ do
    it "returns True if the first list is a subsequence of the second list" $ do
      isSubsequenceOf "GHC" "The Glorious Haskell Compiler" `shouldBe` True
      isSubsequenceOf "JHC" "The Glorious Haskell Compiler" `shouldBe` False
  describe "sortOn" $ do
    it "sorts a list by comparing the results of a key function applied to each element" $ do
      sortOn (>=2) [3,2,1] `shouldBe` [1,3,2]
  describe "uncons" $ do
    it "decomposes a list into its head and tail" $ do
      uncons ""   `shouldBe` Nothing
      uncons "12" `shouldBe` ('1', "2")
