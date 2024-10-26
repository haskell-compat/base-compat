{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | Miscellaneous information about the system environment.
module System.Environment.Compat (
  getArgs
, getProgName
, getEnv
, lookupEnv
, setEnv
, unsetEnv
, withArgs
, withProgName
, getEnvironment
) where

import           System.Environment
