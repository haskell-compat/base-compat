The focus of `base-compat` is to provides the same functionality as the latest
version of `base` for a wider range of compilers.

## What is covered
So far the following is covered.

## For compatibility with the latest released version of `base`

Added:

 * `Text.Read.readMaybe`
 * `Text.Read.readEither`
 * `System.Environment.lookupEnv`
 * `Data.Monoid.<>`

Removed:

 * `System.IO.Error.catch` is not re-exported from Prelude for older versions
   of `base`

## For forward compatibility with the upcoming release of base

Added:

 * `Eq` and `Ord` instance for `ErrorCall`

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
