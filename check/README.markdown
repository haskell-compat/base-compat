# Checking type signatures

Certain modules exported by `base-compat` have type signatures which differ
from older versions of `base`. For example, `when` from `Control.Monad.Compat`
has the type signature

    when :: Applicative f => Bool -> f () -> ()

However, the type signature of `when` from `Control.Monad` from `base-4.7.0.0`
and earlier is actually:

    when :: Monad m => Bool -> m () -> m ()

To make sure that this type signature doesn't accidentally leak out in
`base-compat`, we check the type signatures of exported functions from certain
modules to ensure that they match what is expected.

## Running the type checks

First, download the `typediff` executable (if you haven't already):

    wget https://github.com/haskell-compat/base-compat/releases/download/typediff-0.1.0/typediff
    chmod +x typediff

Then run the test suite, ensuring that `typediff` is added to the user `PATH`
when the tests are run:

    PATH=./:$PATH cabal test
