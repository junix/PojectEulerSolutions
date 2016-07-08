all:
	ghc --make ./Sumfile.hs > /dev/null
	ghc --make ./SumfileByteString.hs > /dev/null
	echo "old string style" > perf.txt
	time ./Sumfile < data >> perf.txt
	echo "byte string style" >> perf.txt
	time ./SumfileByteString < data >> perf.txt
	rm -rf *.o *.hi Sumfile SumfileByteString
