#!/bin/bash

# export OUTPUT_PATH="./out.txt"

cat ./input0.txt | stack runghc ./solution.hs >&2
# cat ./input0.txt | stack runghc ./solution.hs > $OUTPUT_PATH

# cat $OUTPUT_PATH
