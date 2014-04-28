{-# LANGUAGE CPP #-}
module System.Exit.Compat (
  ExitCode(..)
, exitWith
, exitFailure
, exitSuccess
, die
) where

import Prelude
import System.IO

import System.Exit

#if !MIN_VERSION_base(4,7,1)
-- | Write given error message to `stderr` and terminate with `exitFailure`.
--
-- /Since: 4.7.1.0/
die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure
#endif
