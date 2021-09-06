#!/bin/bash

# script for running haskell solutions with minimal modification

cat ./input0.txt | stack runghc ./solution.hs >&2
