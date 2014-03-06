module System.Environment.CompatSpec (main, spec) where

import           Test.Hspec

import           System.Environment.Compat
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
  describe "lookupEnv" $ do
    it "returns specified environment variable" $ do
      withEnv "FOOBAR" "23" $ do
        lookupEnv "FOOBAR" `shouldReturn` Just "23"

    it "returns Nothing if specified environment variable is not set" $ do
      withoutEnv "FOOBAR" $ do
        lookupEnv "FOOBAR" `shouldReturn` Nothing
