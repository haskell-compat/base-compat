{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE OverloadedLists #-}
#endif
module Data.Version.CompatSpec (main, spec) where

import           Test.Hspec
import           Data.Version.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "makeVersion" $ do
    it "constructs a tagless Version" $ do
      makeVersion [1,2,3] `shouldBe` Version [1,2,3] []
#if __GLASGOW_HASKELL__ >= 708
  describe "IsList Version instance" $ do
    it "creates a Version from an Int list" $ do
      [1,2,3] `shouldBe` makeVersion [1,2,3]
#endif