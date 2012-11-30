module System.Environment.CompatSpec (main, spec) where

import           Test.Hspec.Experimental

import           System.Environment.Compat
import           System.SetEnv
import qualified Control.Exception as E

main :: IO ()
main = hspec spec

withEnv :: String -> String -> IO a -> IO a
withEnv k v action = E.bracket save restore $ \_ -> do
  setEnv k v >> action
  where
    save    = lookupEnv k
    restore = maybe (unsetEnv k) (setEnv k)

withoutEnv :: String -> IO a -> IO a
withoutEnv k action = E.bracket save restore $ \_ -> do
  unsetEnv k >> action
  where
    save    = lookupEnv k
    restore = maybe (unsetEnv k) (setEnv k)

spec :: Spec
spec = do
  describe "lookupEnv_compat" $ do
    it "returns specified environment variable" $ do
      withEnv "FOOBAR" "23" $ do
        lookupEnv_compat "FOOBAR" `shouldReturn` Just "23"

    it "returns Nothing if specified environment variable is not set" $ do
      withoutEnv "FOOBAR" $ do
        lookupEnv_compat "FOOBAR" `shouldReturn` Nothing

    it "behaves like lookupEnv" $ \k -> do
      r1 <- lookupEnv_compat k
      r2 <- lookupEnv k
      r1 `shouldBe` r2
