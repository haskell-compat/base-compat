{-# LANGUAGE CPP #-}
module Data.Bits.CompatSpec (main, spec) where

import           Test.Hspec

#if MIN_VERSION_base(4,6,0)
import           Data.Bits.Compat
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec =
#if MIN_VERSION_base(4,6,0)
  describe "Bits Bool instance" $
    it "allows bitwise operations on Bools" $ do
      True  .&. True  `shouldBe` True
      True  .&. False `shouldBe` False
      False .&. True  `shouldBe` False
      False .&. False `shouldBe` False
#else
  return ()
#endif
