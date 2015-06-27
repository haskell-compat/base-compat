module Data.Proxy.CompatSpec (main, spec) where

import           Test.Hspec

import           Data.Proxy.Compat

class TestClass a where
  whoami :: Proxy a -> String

instance TestClass Bool where
  whoami _ = "booly"

instance TestClass Ordering where
  whoami _ = "orderly"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Proxy" $ do
    it "shows correct instance for explicitly given Proxy Bool type" $ do
      whoami (Proxy :: Proxy Bool)     `shouldBe` "booly"
    it "shows correct instance for explicitly given Proxy Ordering type" $ do
      whoami (Proxy :: Proxy Ordering) `shouldBe` "orderly"
    describe "asProxyTypeOf" $ do
    it "check that asProxyTypeOf type forces to pick correct instance of Bounded" $ do
      (minBound `asProxyTypeOf` pBool) `shouldBe` False
      (maxBound `asProxyTypeOf` pBool) `shouldBe` True
  where
    pBool = Proxy :: Proxy Bool

