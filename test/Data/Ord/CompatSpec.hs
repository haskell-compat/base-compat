module Data.Ord.CompatSpec (main, spec) where

import Test.Hspec
import Data.Ord.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Ord (Down a) instance" $
    it "reverses the order in which two values are compared" $
      Down 'a' < Down 'b' `shouldBe` False
