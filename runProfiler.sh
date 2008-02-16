#!/bin/bash

rm -f Tests.tix

ghc -fhpc Tests.lhs --make && ./Tests
hpc markup Tests

rm -f Tests.tix

