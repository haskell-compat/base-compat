module Control.Applicative.CompatSpec (main, spec) where

import           Test.Hspec

import           Control.Applicative.Compat
import           Data.Monoid.Compat

-- simplest one to use
newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
    return a = Identity a
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

  describe "Monad (WrappedMonad m)" $
    it "allows to use a Monad interface in a WrappedMonad" $
      (runIdentity . unwrapMonad
        $  (WrapMonad (return 1 :: Identity Int))
        >> (WrapMonad (return 2 :: Identity Int)))
        `shouldBe` (2::Int)

