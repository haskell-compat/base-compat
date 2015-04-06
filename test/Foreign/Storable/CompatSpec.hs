{-# LANGUAGE CPP #-}
module Foreign.Storable.CompatSpec (main, spec) where

import           Test.Hspec
import           Data.Complex
import           Foreign.Storable.Compat

#if __GLASGOW_HASKELL__ > 702
import           Data.Ratio
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Storable Complex instance" $ do
    it "has twice the sizeOf its realPart" $ do
      sizeOf ((1 :: Double) :+ 2) `shouldBe` 2*sizeOf (1 :: Double)
    it "has the alignment of its realPart" $ do
      alignment ((1 :: Double) :+ 2) `shouldBe` alignment (1 :: Double)
#if __GLASGOW_HASKELL__ > 702
  describe "Storable Ratio instance" $ do
    it "has twice the sizeOf its parameterized type" $ do
      sizeOf ((1 :: Int) % 2) `shouldBe` 2*sizeOf (1 :: Int)
    it "has the alignment of its parameterized type" $ do
      alignment ((1 :: Int) % 2) `shouldBe` alignment (1 :: Int)
#endif
