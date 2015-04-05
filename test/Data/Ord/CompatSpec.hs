module Data.Ord.CompatSpec (main, spec) where

import           Test.Hspec

import           Data.Ord.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Eq (Down a)" $
    it "allows checking if two Down values are equal" $ do
      Down "a" == Down "a" `shouldBe` True
      Down "a" == Down "b" `shouldBe` False

  describe "Ord (ZipList a)" $
    it "allows comparing of two values in reverse sorted order" $ do
      Down "b" >  Down "a" `shouldBe` False
      Down "b" <= Down "a" `shouldBe` True

  describe "Read (ZipList a)" $
    it "allows a Down values to be read from a string" $
      read "Down \"a\"" `shouldBe` Down "a"

  describe "Show (ZipList a)" $
    it "allows a Down values to be converted to a String" $
      show (Down "a") `shouldBe` "Down \"a\""