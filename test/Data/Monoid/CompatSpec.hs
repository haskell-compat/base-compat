module Data.Monoid.CompatSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Applicative
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
  describe "Monoid (Alt f a) instance" $
    it "admits a Monoid from an Alternative instance" $
      property $ \x y z -> do
        Alt x <> Alt (empty :: String) `shouldBe` Alt x
        Alt (empty :: String) <> Alt x `shouldBe` Alt x
        Alt x <> (Alt y <> Alt z) `shouldBe` (Alt x <> Alt y) <> Alt z
