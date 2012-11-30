all:
	cabal clean
	cp base/System/Environment/ExecutablePath.hsc src/System/Environment/ExecutablePath.hsc
	cabal configure --disable-optimization --disable-library-profiling --enable-tests
	cabal build
	cabal test
