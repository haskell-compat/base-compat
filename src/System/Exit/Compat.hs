{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
module System.Exit.Compat (
  ExitCode(..)
, exitWith
, exitFailure
, exitSuccess
, die
) where

import System.Exit

#if !MIN_VERSION_base(4,8,0)

import Prelude.Compat
import System.IO

-- | Write given error message to `stderr` and terminate with `exitFailure`.
--
-- @since 4.8.0.0
die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure
#endif
