## Changes in 0.9.3 [2017.04.10]
 - Sync with `base-4.10`/GHC 8.2
 - Backport `fromLeft`/`fromRight` to `Data.Either.Compat`
 - Backport implementations of `maximumBy`/`minimumBy` which use constant stack
   space to `Data.Foldable.Compat`
 - Backport `asProxyTypeOf` with a generalized type signature to
   `Data.Proxy.Compat`
 - Backport `gcoerceWith` to `Data.Type.Coercion.Compat`
 - Backport `plusForeignPtr` to `Foreign.ForeignPtr.Compat`

## Changes in 0.9.2
 - Allow building on the HaLVM

## Changes in 0.9.1
 - Use the more efficient version of `replicateM` and `replicateM_` introduced
   in `base-4.9`

## Changes in 0.9.0
 - Sync with `base-4.9`/GHC 8.0
 - Weakened `RealFloat` constraints on `realPart`, `imagPart`, `conjugate`,
   `mkPolar`, and `cis` in `Data.Complex.Compat`
 - Backport `Foreign.ForeignPtr.Safe` and `Foreign.Marshal.Safe`
 - Generalize `filterM`, `forever`, `mapAndUnzipM`, `zipWithM`, `zipWithM_`,
   `replicateM`, and `replicateM_` in `Control.Monad` from `Monad` to
   `Applicative`
 - Backport `.Unsafe.Compat` modules (for `Control.Monad.ST`,
   `Control.Monad.ST.Lazy`, `Foreign.ForeignPtr`, and `Foreign.Marshal`)
 - Backport `forkFinally` and `forkOSWithUnmask` to `Control.Concurrent.Compat`
 - Backport `Data.Functor.Const`
 - Backport `modifyIORef'`, `atomicModifyIORef'` and `atomicWriteIORef` to
   `Data.IORef.Compat`
 - `Data.Ratio.{denominator,numerator}` have no `Integral` constraint anymore
 - Backport `modifySTRef'` to `Data.STRef.Compat`
 - Export `String`, `lines`, `words`, `unlines`, and `unwords` to
   `Data.String.Compat`
 - Generalize `Debug.Trace.{traceM, traceShowM}` from `Monad` to `Applicative`
 - Backport `errorWithoutStackTrace` to `Prelude.Compat`
 - Backport `unsafeFixIO` and `unsafeDupablePerformIO` to
   `System.IO.Unsafe.Compat`

## Changes in 0.8.2
 - Backport `bitDefault`, `testBitDefault`, and `popCountDefault` in
   `Data.Bits.Compat` to all versions of `base`
   - Backport `toIntegralSized` to `base-4.7`
 - Backport `nub` and `nubBy` (as well as `union` and `unionBy`, which are
   implemented in terms of them) to fix logic error in `Data.List.Compat`
 - Backport `byteSwap16`, `byteSwap32`, and `byteSwap64` to `Data.Word.Compat`
 - Backport `fillBytes` in `Foreign.Marshal.Utils.Compat`
 - Backport `showFFloatAlt` and `showGFloatAlt` to `Numeric.Compat`

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
