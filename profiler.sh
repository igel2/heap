#!/bin/bash

# funkioniert nicht:

mkdir -p dist2/hpc
ghc --make -fhpc -outputdir dist2 -o dist2/testbinary Test
dist2/testbinary

hpc report --srcdir=. --hpcdir=dist2/hpc --decl-list dist2/testbinary dist2/testbinary.tix Data.Heap Data.Heap.Internal Data.Heap.Item
#hpc report --srcdir=.. --fun-entry-count testbinary ../testbinary.tix Data.Heap Data.Heap.Internal Data.Heap.Item

