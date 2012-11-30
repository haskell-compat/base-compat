{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- | Miscellaneous information about the system environment.
module System.Environment.Compat (
  getArgs
, getProgName
-- , getExecutablePath
, getEnv
, lookupEnv
, withArgs
, withProgName
, getEnvironment

#ifdef TEST
, lookupEnv_compat
#endif
) where

import           System.Environment

#if !MIN_VERSION_base(4,6,0)
lookupEnv :: String -> IO (Maybe String)
lookupEnv = lookupEnv_compat
#endif

lookupEnv_compat :: String -> IO (Maybe String)
lookupEnv_compat k = lookup k `fmap` getEnvironment
