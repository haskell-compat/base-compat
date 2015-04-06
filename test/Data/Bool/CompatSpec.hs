{-# LANGUAGE CPP #-}
module Data.Bool.CompatSpec (main, spec) where

import           Test.Hspec

#if MIN_VERSION_base(4,6,0)
import           Data.Bits
#endif
import           Data.Bool.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
#if MIN_VERSION_base(4,6,0)
  describe "Bits Bool instance" $
    it "allows bitwise operations on Bools" $ do
      True  .&. True  `shouldBe` True
      True  .&. False `shouldBe` False
      False .&. True  `shouldBe` False
      False .&. False `shouldBe` False
#endif

  describe "bool" $ do
    it "evaluates to first parameter if condition is False" $ do
      bool "KO" "OK" False `shouldBe` "KO"

    it "evaluates to second parameter if condition is True" $ do
      bool "KO" "OK" True `shouldBe` "OK"

