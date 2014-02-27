## Changes in 0.4
 - Major refactoring: base-compat no longer aims to replace all base,
   only new code is included in module .compat
 - Removed stubbed modules
 - Removed generation scripts

## Changes in 0.3
 - Added functions from Base 4.7 (bool, isLeft, isRight)
 - Added instances from Base 4.7 (Either Foldable, Traversable,...)

## Changes in 0.2.1
 - Fix build on windows

## Changes in 0.2.0
 - Re-export everything from base
 - provides access to `VERSION_base` and `MIN_VERSION_base` CPP macros (with
   `#include "base-compat.h"`)
 - Do not re-export `System.IO.Error.catch` from `Prelude` for `base` < 4.6.0
 - Add `Eq`/`Ord` instance for `ErrorCall`
 - Remove `GHC.IOBase`, `GHC.Handle`, `Control.Concurrent.QSem`,
   `Control.Concurrent.QSemN`, `Control.Concurrent.SampleVar`, `Data.HashTable`

## Changes in 0.1.0
 - Remove getExecutablePath, it did not work with GHC < 7.2 (patches welcome!)
 - Add `<>`
