all: test.exe

test.exe: test.hs
	ghc test.hs

profiler: FORCE
	ghc test.hs -prof -fprof-auto
	
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