#!/bin/bash

# script for running haskell solutions with minimal modification

export OUTPUT_PATH=./out.txt

cat ./input0.txt | stack runghc ./solution.hs

echo "check for file ./out.txt"

cat ./out.txt
