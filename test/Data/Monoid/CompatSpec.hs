module Data.Monoid.CompatSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Monoid.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "<>" $ do
    it "is an infix synonym for mappend" $ do
      property $ \xs ys -> do
        xs <> ys `shouldBe` (mappend xs ys :: String)
  describe "Num (Sum a) instance" $
    it "allows a Sum value to be created from a number" $
      1 `shouldBe` Sum (1 :: Int)
  describe "Num (Product a) instance" $
    it "allows a Product value to be created from a number" $
      1 `shouldBe` Product (1 :: Int)