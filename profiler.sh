#!/bin/bash

case $1 in
run)	rm -f Tests.tix
	rm -f Data/*.hi Data/*.o
	rm -f Test/*.hi Test/*.o
	rm *.hi *.o
	ghc -fhpc Tests.lhs --make
	./Tests
	hpc report Tests
	hpc markup Tests
	;;
clean)	rm -f Tests.tix
	rm -f Data/*.hi Data/*.o
	rm -f Test/*.hi Test/*.o
	rm -f Tests *.hi *.o
	rm -f hpc_index*.html
	rm -f *.hs.html
	rm -rf .hpc
	;;
esac

