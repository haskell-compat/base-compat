## Changes in 0.8.1.1
 - Fixed Windows build

## Changes in 0.8.1
 - Implement `setEnv` and `unsetEnv` in `System.Environment.Compat` (which were
   ported from the `setenv` package). As a result, `base-compat` now depends
   on `unix` on POSIX-like operating systems.
 - Drop GHC 6.12 (and `base-4.2.0.0`) compatibility

## Changes in 0.8.0.1
 - Retrospective version bump updating the changelog to reflect the changes
   made in 0.8.0

## Changes 0.8.0
 - All orphan instances were split off into a separate package,
   [`base-orphans`](https://github.com/haskell-compat/base-orphans)
 - `base-compat` no longer redefines the data types `Down` and `Alt`. See
   [here](https://github.com/haskell-compat/base-compat/issues/17) for
   the discussion that led to this change.
 - Update `Control.Monad.Compat` for `base-4.8.0.0`
 - Update `Data.List.Compat` for `base-4.8.0.0`
 - Update `Data.Foldable.Compat` for `base-4.8.0.0`

## Changes in 0.7.1
 - Backported `Alt` to `Data.Monoid.Compat`
 - Backported `Down` to `Data.Ord.Compat`

## Changes in 0.7.0
 - Add functions and orphan instances introduced by changes to
   `base-4.7.0.0` and `base-4.8.0.0`

## Changes in 0.6.0
 - Update `Prelude.Compat` for `base-4.8.0.0` and AMP

## Changes in 0.5.0
 - Remove Control.Exception.Base.Compat and GHC.Exception.Compat
 - Add System.Exit.Compat.die
 - Compatibility with base-4.7.1

## Changes in 0.4.1
 - Add `setEnv` and `unsetEnv` to `System.Environment.Compat`

## Changes in 0.4.0
 - Major refactoring: base-compat no longer aims to replace all base,
   only new code is included in module .Compat
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
