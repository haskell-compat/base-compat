all:
	cabal clean
	cabal configure --disable-optimization --disable-library-profiling --enable-tests
	cabal build
	cabal test
