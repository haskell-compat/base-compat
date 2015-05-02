module Numeric.CompatSpec (main, spec) where

import Test.Hspec
import Numeric.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "showFFloatAlt" $
    it "shows a RealFloat value, always using decimal notation" $ do
      showFFloatAlt Nothing  (12 :: Double) "" `shouldBe` "12.0"
      showFFloatAlt (Just 4) (12 :: Double) "" `shouldBe` "12.0000"
  describe "showGFloatAlt" $
    it "shows a RealFloat value, using decimal or scientific notation depending on the value" $ do
      showGFloatAlt Nothing  (12 :: Double) ""         `shouldBe` "12.0"
      showGFloatAlt (Just 4) (12 :: Double) ""         `shouldBe` "12.0000"
      showGFloatAlt Nothing  (1234567890 :: Double) "" `shouldBe` "1.23456789e9"
      showGFloatAlt (Just 4) (1234567890 :: Double) "" `shouldBe` "1.2346e9"
