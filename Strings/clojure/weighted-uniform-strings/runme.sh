#!/bin/bash

export OUTPUT_PATH="./out.txt"

cat ./input0.txt | clj -M ./solution.clj > $OUTPUT_PATH

cat $OUTPUT_PATH
