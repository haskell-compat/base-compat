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

The easiest way to run these tests is to use this command:

    ./run-check.sh

This will ensure that the `typediff` executable is downloaded (if it hasn't
been already) and added to the user `PATH` when the tests are run.
