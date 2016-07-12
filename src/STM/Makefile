all:
	ghc -O2 Rpar.hs -threaded -o Rpar.par
	ghc -O2 Rpar.hs  -o Rpar.seq
clean:
	rm -rf *.o *.hi ./Rpar.par ./Rpar.seq
