module Data.Foldable.CompatSpec (main, spec) where

import           Test.Hspec

import           Data.Foldable.Compat as F
import           Data.Monoid.Compat
import           Control.Applicative.Compat (Const(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Either Foldable Instance" $ do
    it "foldMap returns mempty for a Left value" $
      foldMap (<> "+") (Left "abc" :: Either String String) `shouldBe` mempty
    it "foldMap returns the result of the function on the Right value" $
      foldMap (<> "+") (Right "abc" :: Either String String) `shouldBe` "abc+"

    it "foldr returns the accumulator for a Left value" $
      F.foldr (<>) "+" (Left "abc" :: Either String String) `shouldBe` "+"
    it "foldr returns the result of the function on the Right value and accumulator" $
      F.foldr (<>) "+" (Right "abc" :: Either String String) `shouldBe` "abc+"

  describe "(,) Foldable Instance" $ do
    it "foldMap returns the result of the function applied to the second element" $
      foldMap (<> "+") ("xyz","abc") `shouldBe` "abc+"

    it "foldr returns the result of the function on the second element of the tuple and accumulator" $
      F.foldr (<>) "+" ("xyz","abc") `shouldBe` "abc+"

  describe "Const m Foldable Instance" $ do
    it "foldMap always returns mempty" $
      foldMap (<> "+") (Const "abc") `shouldBe` ""
    it "foldr applies the function to the accumulator and mempty" $ do
      F.foldr (<>) "+" (Const "abc") `shouldBe` "+"



