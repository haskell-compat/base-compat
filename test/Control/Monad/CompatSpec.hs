module Control.Monad.CompatSpec (main, spec) where

import           Test.Hspec

import           Control.Monad.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "void" $ do
    it "discards the return value of an action" $ do
      void (return True) `shouldBe` Just ()
  describe "(<$!>" $ do
    it "is a strict version of (<$>)" $ do
      not <$!> [True, False] `shouldBe` not <$> [True, False]