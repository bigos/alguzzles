#!/bin/bash

export OUTPUT_PATH=./out.txt

> ./out.txt
cat ./input0.txt | clojure -M ./solution.clj
cat ./out.txt
