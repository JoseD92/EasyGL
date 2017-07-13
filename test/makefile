GHC = ghc
GHCFLAGS = $(PACKAGE) --make
PACKAGE = -package-db=../.cabal-sandbox/x86_64-windows-ghc-8.0.2-packages.conf.d
PROFILEFLAGS = -prof -fprof-auto

all: test.exe

test.exe: FORCE
	$(GHC) $(GHCFLAGS) test.hs

profiler: FORCE
	$(GHC) $(GHCFLAGS) test.hs $(PROFILEFLAGS)
	
runProf: FORCE
	./test +RTS -p
	hp2ps test.hp
	rm -f test.pdf
	ps2pdf test.ps

runStack: FORCE
	./test +RTS -hc
	hp2ps test.hp
	rm -f test.pdf
	ps2pdf test.ps

runClou: FORCE
	./test +RTS -hd
	hp2ps test.hp
	rm -f test.pdf
	ps2pdf test.ps
	
FORCE: ;

clean: 
	rm -rf *.o *.hi *.exe *.pdf *.aux *.hp *.prof *.ps