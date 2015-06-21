# A compatibility layer for `base` [![Hackage version](https://img.shields.io/hackage/v/base-compat.svg?style=flat)](http://hackage.haskell.org/package/base-compat) [![Build Status](https://img.shields.io/travis/haskell-compat/base-compat.svg?style=flat)](https://travis-ci.org/haskell-compat/base-compat)
## Scope

The scope of `base-compat` is to provide functions available in later versions
of base to a wider (older) range of compilers.

In addition, successful library proposals that have been accepted to be part of
upcoming versions of `base` are also included.  This package is not intended to
replace `base`, but to complement it.

Note that `base-compat` does not add any orphan instances.  There is a separate
package [`base-orphans`](https://github.com/haskell-compat/base-orphans) for
that.

## Basic usage

In your cabal file, you should have something like this:

```
  build-depends:      base              >= 4.3
                    , base-compat       >= 0.8.0
```

Then, lets say you want to use the `isRight` function introduced with
`base-4.7.0.0`.  Replace:

```
import Data.Either
```

with

```
import Data.Either.Compat
```

_Note (1)_: There is no need to import both unqualified.  The `.Compat` modules
re-exports the original module.

_Note (2)_: If a given module `.Compat` version is not defined, that either
means that:

* The module has not changed in recent base versions, thus no `.Compat` is
  needed.
* The module has changed, but the changes depend on newer versions of GHC, and
  thus are not portable.
* The module has changed, but those changes have not yet been merged in
  `base-compat`: patches are welcomed!

## Using `Prelude.Compat`

If you want to use `Prelude.Compat` (which provides all the
AMP/Traversable/Foldable changes from `base-4.8.0.0`), it's best to hide
`Prelude`, e.g.:

    import Prelude ()
    import Prelude.Compat

    main :: IO ()
    main = mapM_ print (Just 23)

Alternatively, you can use the `NoImplicitPrelude` language extension:

    {-# LANGUAGE NoImplicitPrelude #-}
    import Prelude.Compat

    main :: IO ()
    main = mapM_ print (Just 23)

Note that we use

    mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()

from `Data.Foldable` here, which is only exposed from `Prelude` since
`base-4.8.0.0`.

Using this approach allows you to write code that works seamlessly with all
versions of GHC that are supported by `base-compat`.

## What is covered
So far the following is covered.

### For compatibility with the latest released version of `base`

 * `Prelude.Compat` incorporates the AMP/Foldable/Traversable changes and
   exposes the same interface as `Prelude` from `base-4.8.0.0`
 * `System.IO.Error.catch` is not re-exported from `Prelude.Compat` for older
   versions of `base`
 * `Text.Read.Compat.readMaybe`
 * `Text.Read.Compat.readEither`
 * `Data.Monoid.Compat.<>`
 * Added `bitDefault`, `testBitDefault`, and `popCountDefault` to `Data.Bits.Compat`
 * Added `toIntegralSized` to `Data.Bits.Compat` (if using `base-4.7`)
 * Added `bool` function to `Data.Bool.Compat`
 * Added `isLeft` and `isRight` to `Data.Either.Compat`
 * Added `forkFinally` to `Control.Concurrent.Compat`
 * Added `withMVarMasked` function to `Control.Concurrent.MVar.Compat`
 * Added `(<$!>)` function to `Control.Monad.Compat`
 * Added `($>)` and `void` functions to `Data.Functor.Compat`
 * `(&)` function to `Data.Function.Compat`
 * `($>)` and `void` functions to `Data.Functor.Compat`
 * `modifyIORef'`, `atomicModifyIORef'` and `atomicWriteIORef` to `Data.IORef.Compat`
 * `dropWhileEnd`, `isSubsequenceOf`, `sortOn`, and `uncons` functions to `Data.List.Compat`
 * Correct versions of `nub`, `nubBy`, `union`, and `unionBy` to `Data.List.Compat`
 * `modifySTRef'` to `Data.STRef.Compat`
 * `String`, `lines`, `words`, `unlines`, and `unwords` to `Data.String.Compat`
 * `makeVersion` function to `Data.Version.Compat`
 * `traceId`, `traceShowId`, `traceM`, and `traceShowM` functions to `Debug.Trace.Compat`
 * `byteSwap16`, `byteSwap32`, and `byteSwap64` to `Data.Word.Compat`
 * `calloc` and `callocBytes` functions to `Foreign.Marshal.Alloc.Compat`
 * `callocArray` and `callocArray0` functions to `Foreign.Marshal.Array.Compat`
 * `fillBytes` to `Foreign.Marshal.Utils.Compat`
 * Added `Data.List.Compat.scanl'`
 * `showFFloatAlt` and `showGFloatAlt` to `Numeric.Compat`
 * `lookupEnv`, `setEnv` and `unsetEnv` to `System.Environment.Compat`
 * `unsafeFixIO` and `unsafeDupablePerformIO` to `System.IO.Unsafe.IO`

## Supported versions of GHC/base

 * `ghc-7.10.1` / `base-4.8.0.0`
 * `ghc-7.8.4`  / `base-4.7.0.2`
 * `ghc-7.8.3`  / `base-4.7.0.1`
 * `ghc-7.8.2`  / `base-4.7.0.0`
 * `ghc-7.8.1`  / `base-4.7.0.0`
 * `ghc-7.6.3`  / `base-4.6.0.1`
 * `ghc-7.6.2`  / `base-4.6.0.1`
 * `ghc-7.6.1`  / `base-4.6.0.0`
 * `ghc-7.4.2`  / `base-4.5.1.0`
 * `ghc-7.4.1`  / `base-4.5.0.0`
 * `ghc-7.2.2`  / `base-4.4.1.0`
 * `ghc-7.2.1`  / `base-4.4.0.0`
 * `ghc-7.0.4`  / `base-4.3.1.0`
 * `ghc-7.0.3`  / `base-4.3.1.0`
 * `ghc-7.0.2`  / `base-4.3.1.0`
 * `ghc-7.0.1`  / `base-4.3.0.0`

Patches are welcome; add tests for new code!
