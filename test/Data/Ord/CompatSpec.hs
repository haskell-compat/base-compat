{-# LANGUAGE CPP #-}
module Data.Ord.CompatSpec (main, spec) where

import           Test.Hspec

#if MIN_VERSION_base(4,6,0)
import           Data.Ord.Compat
#endif

main :: IO ()
main = hspec spec

spec :: Spec
#if MIN_VERSION_base(4,6,0)
spec = do
  describe "Eq (Down a)" $
    it "allows checking if two Down values are equal" $ do
      Down "a" == Down "a" `shouldBe` True
      Down "a" == Down "b" `shouldBe` False

  describe "Ord (Down a)" $
    it "allows comparing of two values in reverse sorted order" $ do
      Down "b" >  Down "a" `shouldBe` False
      Down "b" <= Down "a" `shouldBe` True

  describe "Read (Down a)" $
    it "allows a Down values to be read from a string" $
      read "Down \"a\"" `shouldBe` Down "a"

  describe "Show (Down a)" $
    it "allows a Down values to be converted to a String" $
      show (Down "a") `shouldBe` "Down \"a\""
#else
spec = return ()
#endif
