all: FORCE
	make safe
	cabal haddock --hyperlink-source
	
safe: FORCE
	(cabal install --enable-library-profiling || make safe)
	
remake: FORCE
	rm -rf dist
	make all

reset: FORCE
	rm -rf dist
	rm -rf cabal.sandbox.config
	rm -rf .cabal-sandbox
	cabal sandbox init
	make all

documentation: FORCE
	
	
FORCE: ;