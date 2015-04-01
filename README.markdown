# A compatibility layer for `base`
## Scope

The scope of `base-compat` is to provide functions, data types and instances
available in later versions of base to a wider (older) range of compilers.

In addition, successful library proposals that have been accepted to be part of
upcoming versions of `base` are also included.  This package is not intended to
replace `base`, but to complement it.

**Note:** `base-compat` never redefines data types or classes, it only adds
missing functions, data types and instances.  This implies that `base-compat`
is a best-effort approach.  If a data type or class is changed in a backward
incompatible way, `base-compat` may not be able to address this.

## Basic usage

In your cabal file, you should have something like this:

```
  build-depends:      base              == 4.*
                    , base-compat       >= 0.6
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
 * `System.Environment.Compat.lookupEnv`
 * `Data.Monoid.Compat.<>`
 * Added `Eq` and `Ord` instance for `Contro.Exception.ErrorCall`
 * Added `Monoid` instance for `Const`
 * Added `Monad` instance for `WrappedMonad`
 * Added `bool` function to `Data.Bool.Compat`
 * Added `isLeft` and `isRight` to `Data.Either.Compat`
 * `Foldable` instance for `Either`, `(,)` and `Const`
 * `Storable` instance for `Complex` and `Ratio`
 * `Traversable` instance for `Either`, `(,)` and `Const`
 * `Version` instance for `IsList`
 * Added `(<$!>)` function to `Control.Monad.Compat`
 * Added `($>)` and `void` functions to `Data.Functor.Compat`
 * `(&)` function to `Data.Function.Compat`
 * `($>)` and `void` functions to `Data.Functor.Compat`
 * `dropWhileEnd`, `isSubsequenceOf`, `sortOn`, and `uncons` functions to `Data.List.Compat`
 * `makeVersion` function to `Data.Version.Compat`

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
 * `ghc-6.12.3` / `base-4.2.0.2`
 * `ghc-6.12.2` / `base-4.2.0.1`
 * `ghc-6.12.1` / `base-4.2.0.0`

Patches are welcome; add tests for new code!

## Development

For `Prelude.Compat` there is an `Prelude.index` file that was generated from
the output of

    ghc --show-iface Prelude.hi

To verify that `Prelude.Compat` matches the specification given in `Prelude.types` run:

    ./check-Prelude.sh
