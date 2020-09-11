;; Complete the quickSort function below.

(ns hack.solution)

(defn quickSort [arr]


  )

(def fptr (get (System/getenv) "OUTPUT_PATH"))

(def n (Integer/parseInt (clojure.string/trim (read-line))))

(def arr (vec (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))))

(def result (quickSort arr))

(spit fptr (clojure.string/join " " result) :append true)
(spit fptr "\n" :append true)

;;; ---- do not copy below code to hackerrank

(defn solver []
  (quicksort [4 3 5 7 2]))

;;; ruby version
;; #!/bin/ruby

;; require 'json'
;; require 'stringio'

;; # Complete the quickSort function below.
;; def quickSort(arr)
;; end

;; fptr = File.open(ENV['OUTPUT_PATH'], 'w')
;; n = gets.to_i
;; arr = gets.rstrip.split(' ').map(&:to_i)

;; result = quickSort arr
;; fptr.write result.join " "
;; fptr.write "\n"
;; fptr.close()
;;;
