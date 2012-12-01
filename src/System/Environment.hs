module System.Environment (
  module Base
, lookupEnv
) where

import "base" System.Environment as Base

#if !MIN_VERSION_base(4,6,0)
-- | Return the value of the environment variable @var@, or @Nothing@ if
-- there is no such value.
--
-- For POSIX users, this is equivalent to 'System.Posix.Env.getEnv'.
lookupEnv :: String -> IO (Maybe String)
lookupEnv k = lookup k `fmap` getEnvironment
#endif
