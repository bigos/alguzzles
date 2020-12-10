#!/bin/bash

export OUTPUT_PATH=./out.txt

cat ./input0.txt | clj -Srepro ./solution.clj

cat ./out.txt
