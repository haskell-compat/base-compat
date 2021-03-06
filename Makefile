current:
	ghc --version
	cabal clean
	cabal install --only-dependencies --enable-tests
	cabal configure --disable-optimization --disable-library-profiling --enable-tests
	cabal build
	cabal test
all:
	env-ghc-7.8.sh   make current
	env-ghc-7.6.2.sh make current
	env-ghc-7.6.1.sh make current
	env-ghc-7.4.2.sh make current
	env-ghc-7.4.1.sh make current
	env-ghc-7.2.2.sh make current
	env-ghc-7.2.1.sh make current
	env-ghc-7.0.4.sh make current
	env-ghc-7.0.3.sh make current
	env-ghc-7.0.2.sh make current
	env-ghc-7.0.1.sh make current

install:
	cabal clean
	cabal install --disable-library-profiling --disable-documentation
