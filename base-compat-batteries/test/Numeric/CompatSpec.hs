module Numeric.CompatSpec (main, spec) where

import Test.Hspec
import Numeric.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "showFFloatAlt" $ do
    it "shows a RealFloat value, always using decimal notation" $
      showFFloatAlt Nothing  (12 :: Double) "" `shouldBe` "12.0"
    it "allows to specify the number of decimal places" $
      showFFloatAlt (Just 4) (12 :: Double) "" `shouldBe` "12.0000"
  describe "showGFloatAlt" $ do
    it "shows a RealFloat value, using decimal notation if the absolute value lies between 0.1 and 9,999,999" $
      showGFloatAlt Nothing  (12 :: Double) ""         `shouldBe` "12.0"
    it "shows a RealFloat value, using decimal notation and specifying the number of decimal places" $
      showGFloatAlt (Just 4) (12 :: Double) ""         `shouldBe` "12.0000"
    it "shows a RealFloat value, using scientific notation if the absolute value falls outside of the range" $
      showGFloatAlt Nothing  (1234567890 :: Double) "" `shouldBe` "1.23456789e9"
    it "shows a RealFloat value, using scientific notation and specifying the number of decimal places" $
      showGFloatAlt (Just 4) (1234567890 :: Double) "" `shouldBe` "1.2346e9"
  describe "readBin" $ do
    it "parses an entirely binary Integer" $
      readBinInteger "00000111" `shouldBe` [(7, "")]
    it "does not parse a non-binary Integer" $
      readBinInteger "-24" `shouldBe` []
    it "parses the binary prefix of an Integer" $
      readBinInteger "1011784372843778438743" `shouldBe` [(11,"784372843778438743")]
  describe "showBin" $ do
    it "shows small Ints in base 2" $
      map (\ x -> showBinInt x "") [1..32] `shouldBe`
        [ "1","10","11","100","101","110","111","1000","1001","1010","1011","1100","1101","1110","1111"
        , "10000","10001","10010","10011","10100","10101","10110","10111","11000"
        , "11001","11010","11011","11100","11101","11110","11111","100000" ]
    it "shows a large Int in base 2" $
      showBinInt 241324784 "" `shouldBe` "1110011000100101001011110000"
  where
    readBinInteger :: ReadS Integer
    readBinInteger = readBin

    showBinInt :: Int -> ShowS
    showBinInt = showBin
