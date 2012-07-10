all:
	cabal build
	doctest MyFirstLoop.hs
	cabal hscolour --executables
	cabal haddock --executables '--haddock-options=--source-base=src/ --source-module=src/%{MODULE/./-}.html --source-entity=src/%{MODULE/./-}.html#%N' 

configure:
	cabal configure --enable-tests
	cabal install --only-dependencies --enable-tests
