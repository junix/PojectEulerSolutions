all:
	ghc -O2 Rpar.hs -threaded -o Rpar.par
	ghc -O2 Rpar.hs  -o Rpar.seq
clean:
	gfind . -type f -executable -delete
	gfind . -name '*.o' -delete  -or -name '*.hi' -delete
