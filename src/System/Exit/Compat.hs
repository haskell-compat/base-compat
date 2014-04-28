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

-- | Write given error message to `stderr` and terminate with `exitFailure`.
--
-- /Since: 4.7.1.0/
die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure
