{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE OverloadedLists #-}
#endif
module Data.Version.CompatSpec (main, spec) where

import           Test.Hspec
import           Data.Data
import           Data.Version.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "makeVersion" $
    it "constructs a tagless Version" $
      makeVersion [1,2,3] `shouldBe` Version [1,2,3] []

  describe "Data Version instance" $
    it "allows obtaining a Version constructor" $
      dataTypeName (dataTypeOf (makeVersion [1,2,3])) `shouldBe` "Data.Version.Version"

#if MIN_VERSION_base(4,7,0)
  describe "IsList Version instance" $
    it "creates a Version from an Int list" $
      [1,2,3] `shouldBe` makeVersion [1,2,3]
#endif
