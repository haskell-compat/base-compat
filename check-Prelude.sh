#!/bin/bash
runhaskell dumptypes.hs | typediff Prelude.types -
