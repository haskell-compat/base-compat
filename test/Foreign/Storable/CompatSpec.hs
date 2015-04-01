module Foreign.Storable.CompatSpec (main, spec) where

import           Test.Hspec
import           Data.Complex
import           Data.Ratio
import           Foreign.Storable.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Storable Complex instance" $ do
    it "has twice the sizeOf its realPart" $ do
      sizeOf ((1 :: Int) :+ 2) `shouldBe` 2*sizeOf (1 :: Int)
    it "has the alignment of its realPart" $ do
      alignment ((1 :: Int) :+ 2) `shouldBe` alignment (1 :: Int)
  describe "Storable Ratio instance" $ do
    it "has twice the sizeOf its parameterized type" $ do
      sizeOf ((1 :: Int) % 2) `shouldBe` 2*sizeOf (1 :: Int)
    it "has the alignment of its parameterized type" $ do
      alignment ((1 :: Int) % 2) `shouldBe` alignment (1 :: Int)