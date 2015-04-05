module Data.Word.CompatSpec (main, spec) where

import           Test.Hspec

import           Data.Word.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "byteSwap16" $
    it "reverses byte order in a Word16" $ do
      byteSwap16 1   `shouldBe` 256
      byteSwap16 256 `shouldBe` 1
      byteSwap16 257 `shouldBe` 257
  describe "byteSwap32" $
    it "reverses byte order in a Word32" $ do
      byteSwap32 1        `shouldBe` 16777216
      byteSwap32 16777216 `shouldBe` 1
      byteSwap32 16777217 `shouldBe` 16777217
  describe "byteSwap64" $
    it "reverses byte order in a Word64" $ do
      byteSwap64 1                 `shouldBe` 72057594037927936
      byteSwap64 72057594037927936 `shouldBe` 1
      byteSwap64 72057594037927937 `shouldBe` 72057594037927937