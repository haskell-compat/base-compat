#!/bin/bash
runhaskell dumptypes.hs | ./typediff/typediff Prelude.types -
