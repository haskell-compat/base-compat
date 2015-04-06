module Foreign.Marshal.Alloc.CompatSpec (main, spec) where

import           Test.Hspec

import           Foreign.Marshal.Alloc.Compat
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "calloc" $
    it "allocates memory with bytes of value zero" $ do
        intPtr1 <- calloc :: IO (Ptr Int)
        intPtr2 <- new 0
        val1 <- peek intPtr1
        val2 <- peek intPtr2
        val1 `shouldBe` val2
        free intPtr1
        free intPtr2
