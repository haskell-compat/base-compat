{-# LANGUAGE TupleSections, NoImplicitPrelude #-}
module Data.Traversable.CompatSpec (main, spec) where

import           Prelude.Compat
import           Test.Hspec

import           Control.Applicative.Compat (Const(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Either Traversable Instance" $ do
    it "traverses a Left value" $
      traverse (:[]) (Left 5 :: Either Int String) `shouldBe` [Left 5]
    it "traverses a Right Value" $
      traverse (:[]) (Right "aaa" :: Either Int String) `shouldBe` [Right "aaa"]

  describe "(,) a Traversable Instance" $ do
    it "traverses a (,) a value" $
      traverse (:[]) (5::Int,"aaa") `shouldBe` [(5,"aaa")]

  describe "Const m Traversable Instance" $ do
    it "traverses a Const a value" $ do
      fmap getConst (traverse (:[]) (Const 5 :: Const Int String)) `shouldBe` [5]

