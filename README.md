# README #

In order to use this repo you need Common Lisp and Haskell to be installed on your machine

### What is this repository for? ###

* Solving programming puzzles


### Running haskell programs  ###

    cat ./input0.txt | stack runghc ./solution.hs >&2

### Running clojure programs  ###

Install lein-exec:
https://github.com/kumarshantanu/lein-exec

run in terminal

    $ cat ./input0.txt | lein exec ./solution.clj
