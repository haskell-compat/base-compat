module TypeDiffSpec (main, spec) where

import           Test.Hspec

import           Data.Map (fromList)
import           Language.Haskell.Exts.Parser
import           TypeDiff

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sigMap" $ do
    it "creates mapping from function names to type signatures" $ do
      sigMap "foo :: Int\nbar :: Float" `shouldBe` fromList [("foo", "Int"), ("bar", "Float")]

  describe "typeDiff" $ do
    it "returns the empty string on success" $ do
      let sigs1 = "foo :: Int"
          sigs2 = "foo :: Int"
      typeDiff sigs1 sigs2 `shouldBe` ""

    it "detects type missmatches" $ do
      let sigs1 = "foo :: Float"
          sigs2 = "foo :: Int"
      typeDiff sigs1 sigs2 `shouldBe` "wrong types:\n  foo :: Float\n  foo :: Int\n"

    it "detects missing identifiers" $ do
      let sigs1 = "foo :: a\nbar :: a"
          sigs2 = "foo :: a"
      typeDiff sigs1 sigs2 `shouldBe` "missing bar\n"

    it "detects extra identifiers" $ do
      let sigs1 = "foo :: a"
          sigs2 = "foo :: a\nbar :: a"
      typeDiff sigs1 sigs2 `shouldBe` "extra bar\n"

    it "ignores order of constrains" $ do
      let sigs1 = "truncate :: (Integral b, RealFrac a) => a -> b"
          sigs2 = "truncate :: (RealFrac a, Integral b) => a -> b"
      typeDiff sigs1 sigs2 `shouldBe` ""

  describe "typeEq" $ do
    it "returns False for different types" $ do
      let ParseOk x = parseType "Int -> Int"
          ParseOk y = parseType "Float -> Float"
      typeEq x y `shouldBe` False

    it "returns True for same types" $ do
      let ParseOk x = parseType "a -> a"
          ParseOk y = parseType "a -> a"
      typeEq x y `shouldBe` True

    it "ignores order of constrains" $ do
      let ParseOk x = parseType "(Integral b, RealFrac a) => a -> b"
          ParseOk y = parseType "(RealFrac a, Integral b) => a -> b"
      typeEq x y `shouldBe` True

    it "performs alpha conversion" $ do
      let ParseOk x = parseType "a -> b"
          ParseOk y = parseType "b -> a"
      typeEq x y `shouldBe` True

    it "ignores redundant parentheses for constrains" $ do
      let ParseOk x = parseType "Floating a => a -> a"
          ParseOk y = parseType "(Floating a) => a -> a"
      typeEq x y `shouldBe` True

  describe "alphaNormalize" $ do
    it "performs alpha conversion" $ do
      let ParseOk x = parseType "a -> a -> b"
          ParseOk y = parseType "t0 -> t0 -> t1"
      alphaNormalize x `shouldBe` y

    it "leaves concrete types intact" $ do
      let ParseOk x = parseType "Int -> Float"
          ParseOk y = parseType "Int -> Float"
      alphaNormalize x `shouldBe` y

  describe "normalizeConstrainNames" $ do
    it "normalizes qualified name for Applicative" $ do
      let ParseOk x = parseType "GHC.Base.Applicative f => f a"
          ParseOk y = parseType "Control.Applicative.Applicative f => f a"
      normalizeConstrainNames x `shouldBe` y

    it "normalizes qualified name for Alternative" $ do
      let ParseOk x = parseType "GHC.Base.Alternative f => f a"
          ParseOk y = parseType "Control.Applicative.Alternative f => f a"
      normalizeConstrainNames x `shouldBe` y

    it "normalizes qualified name for MonadPlus" $ do
      let ParseOk x = parseType "GHC.Base.MonadPlus m => m a"
          ParseOk y = parseType "Control.Monad.MonadPlus m => m a"
      normalizeConstrainNames x `shouldBe` y


    it "normalizes qualified name for Maybe" $ do
      let ParseOk x = parseType "GHC.Base.Maybe a"
          ParseOk y = parseType "Data.Maybe.Maybe a"
      normalizeConstrainNames x `shouldBe` y

    it "normalizes qualified name for Monoid" $ do
      let ParseOk x = parseType "GHC.Base.Monoid m => m"
          ParseOk y = parseType "Data.Monoid.Monoid m => m"
      normalizeConstrainNames x `shouldBe` y

    it "normalizes qualified name for Bool" $ do
      let ParseOk x = parseType "GHC.Types.Ordering"
          ParseOk y = parseType "GHC.Ordering.Ordering"
      normalizeConstrainNames x `shouldBe` y
