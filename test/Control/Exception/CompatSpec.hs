module Control.Exception.CompatSpec (main, spec) where

import           Test.Hspec

import           Control.Exception.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ErrorCall" $ do
    it "has an Eq instance" $ do
      ErrorCall "foo" `shouldBe` ErrorCall "foo"

    it "has an Ord instance" $ do
      ErrorCall "foo" `shouldSatisfy` (> ErrorCall "bar")
