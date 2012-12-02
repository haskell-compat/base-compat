module Control.ExceptionSpec (main, spec) where

import           Test.Hspec

import           Control.Exception

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "catch" $ do
    it "catches an exception" $ do
      -- The main purpose of this test is to check that `catch` is not
      -- ambiguated by Prelude.catch with older versions of base.
      (throwIO DivideByZero) `catch` (`shouldBe` DivideByZero)

  describe "ErrorCall" $ do
    it "has an Eq instance" $ do
      ErrorCall "foo" `shouldBe` ErrorCall "foo"

    it "has an Ord instance" $ do
      ErrorCall "foo" `shouldSatisfy` (> ErrorCall "bar")
