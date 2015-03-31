# A compatibility layer for `base`
## Scope

The scope of `base-compat` is to provide functions available in later
versions of base to a wider (older) range of compilers.

In addition, successful library proposals that have been accepted to be 
part of upcoming versions of `base` are also included. 
This module is not intended to replace `base`, but to complement it.

## Usage

In your cabal file, you should have something like this:

```
  build-depends:      base              >= 4.5
                    , base-compat       >= 0.4
```

Then, lets say you want to use the `isRight` function defined in base-4.7.
Replace:

```
import Data.Either
```

with

```
import Data.Either.Compat
```

_Note (1)_: do not import both unqualified, as you will have name conflicts.
The `.Compat` modules re-exports the original module.

_Note (2)_: if a given module `.Compat` version is not defined, that either
means that:
	
* The module has not changed in recent base versions, thus no `.Compat`
  is needed.
* The module has changed, but the changes depend on newer versions of 
  GHC, and thus are not portable.
* The module has changed, but those changes have not yet been merged in
  `base-compat`: patches are welcomed!


## What is covered
So far the following is covered.

### For compatibility with the latest released version of `base`

Added:

 * `Text.Read.readMaybe`
 * `Text.Read.readEither`
 * `System.Environment.lookupEnv`
 * `Data.Monoid.<>`

Removed:

 * `System.IO.Error.catch` is not re-exported from Prelude for older versions
   of `base`

### For forward compatibility with base-4.5

Added:
 * `dropWhileEnd` function to `Data.List`

### For forward compatibility with the upcoming release of base (4.8)

Added:

 * `Eq` and `Ord` instance for `ErrorCall`
 * `Monoid` instance for `Const`
 * `Monad` instance for `WrappedMonad`
 * `bool` function to `Data.Bool`
 * `isLeft` and `isRight` to `Data.Either`
 * `Either`, `(,)` and `Const` instances for `Foldable`
 * `Either`, `(,)` and `Const` instances for `Traversable`
 * `(&)` function to `Data.Function`
 * `($>)` and `void` functions to `Data.Functor`
 * `isSubsequenceOf`, `sortOn`, and `uncons` functions to `Data.List`


Removed:

 * `GHC.IOBase`
 * `GHC.Handle`
 * `Control.Concurrent.QSem`
 * `Control.Concurrent.QSemN`
 * `Control.Concurrent.SampleVar`
 * `Data.HashTable`

## Supported versions of GHC/base

 * `ghc-7.6.1` / `base-4.6.0.0`
 * `ghc-7.4.2` / `base-4.5.1.0`
 * `ghc-7.4.1` / `base-4.5.0.0`
 * `ghc-7.2.2` / `base-4.4.1.0`
 * `ghc-7.2.1` / `base-4.4.0.0`
 * `ghc-7.0.4` / `base-4.3.1.0`
 * `ghc-7.0.3` / `base-4.3.1.0`
 * `ghc-7.0.2` / `base-4.3.1.0`
 * `ghc-7.0.1` / `base-4.3.0.0`
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
