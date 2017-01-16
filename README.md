# README #

In order to use this repo you need Common Lisp and Haskell to be installed on your machine

### What is this repository for? ###

* Solving programming puzzles


### Running haskell programs  ###

#### compiled ####

compile your code

    $ ghc ./solution.hs

run it in terminal

    $ cat ./input1.txt | ./solution

#### using runhaskell ###

    $ cat ./input0.txt | runhaskell ./solution.hs


### Running clojure programs  ###

Install lein-exec:
https://github.com/kumarshantanu/lein-exec

run in terminal

    $ cat ./input0.txt | lein exec ./solution.clj
