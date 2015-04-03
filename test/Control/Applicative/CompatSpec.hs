{-# LANGUAGE DeriveFunctor #-}
module Control.Applicative.CompatSpec (main, spec) where

import           Test.Hspec

import           Control.Applicative.Compat
import           Data.Monoid.Compat

-- simplest one to use
newtype Identity a = Identity { runIdentity :: a }
  deriving Functor

instance Applicative Identity where
  pure     = Identity
  Identity f <*> x = f <$> x

instance Monad Identity where
  return = Identity
  m >>= k  = k (runIdentity m)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Monoid (Const a b)" $ do
    it "mempty returns an empty const" $
      getConst (mempty :: (Const String Int)) `shouldBe` ""
    it "mappends const part" $
      getConst ((Const "aaa" :: Const String Int) <> (Const "bbb" :: Const String Int))
        `shouldBe` "aaabbb"

  describe "Eq (Const a b)" $
    it "allows checking if two Const values are equal" $ do
      (Const "a" :: Const String Int) == Const "a" `shouldBe` True
      (Const "a" :: Const String Int) == Const "b" `shouldBe` False

  describe "Ord (Const a b)" $
    it "allows comparing of two Const values" $ do
      (Const 'b' :: Const Char Int) >  Const 'a' `shouldBe` True
      (Const 'b' :: Const Char Int) <= Const 'a' `shouldBe` False

  describe "Read (Const a b)" $
    it "allows a Const value to be read from a string" $
      read "Const 1" `shouldBe` (Const 1 :: Const Int Int)

  describe "Show (Const a b)" $
    it "allows a Const value to be converted to a String" $
      show (Const 1 :: Const Int Int) `shouldBe` "Const 1"

  describe "Monad (WrappedMonad m)" $
    it "allows to use a Monad interface in a WrappedMonad" $
      (runIdentity . unwrapMonad
        $  (WrapMonad (return 1 :: Identity Int))
        >> (WrapMonad (return 2 :: Identity Int)))
        `shouldBe` (2::Int)

