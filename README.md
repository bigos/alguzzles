# README #

In order to use this repo you need Common Lisp and Haskell to be installed on your machine

### What is this repository for? ###

* Solving programming puzzles


### Running haskell programs  ###

    cat ./input0.txt | stack runghc ./solution.hs >&2

### Running clojure programs  ###

run in terminal

    cat ./input0.txt | clojure -M ./solution.clj


Emacs REPL

Using the runme.sh script and modified solution code we can print the arguments
needed to solve the problem. At this point we can use the usual REPL workflow to
solve the puzzle.

* Open the clojure file
* M-x cider-jack-in
* In the clojure file do C-c M-z
* Play with the REPL
