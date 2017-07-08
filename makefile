all: FORCE
	cabal install --enable-library-profiling
	cabal haddock --hyperlink-source
	
remake: FORCE
	rm -rf dist
	make all

reset: FORCE
	rm -rf dist
	rm -rf cabal.sandbox.config
	rm -rf .cabal-sandbox
	cabal sandbox init
	cabal install --enable-library-profiling
	cabal haddock --hyperlink-source

documentation: FORCE
	
	
FORCE: ;