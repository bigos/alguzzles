;; solution in Clojure
(use 'clojure.java.io 'clojure.pprint)

(defn exp [x n]
  (reduce * (repeat n x)))

(def limit (* 4 (exp 10 16)))

(defn fib [n a]
  (if (or (> (first a) n)
          (> (first a) limit))
    (drop 3 (reverse (rest a)))
    (recur n (cons (+ (first a) (second a)) a))))

(def all-fib (fib limit '(0 1)))
(def even-fib (filter even? all-fib))

(defn zzz [l a ]
  (if (= (count l) 0)
    (reverse a)
    (recur (rest l)
           (cons (+ (first l) (first a)) a))))

;;; close to the solution
(def sums (map (fn [x y] (list x y)) even-fib (rest (zzz even-fib '(0)))))
(def sum-keys (map first sums))
(def hash-sums (reduce (fn [a x] (update a (first x) (fn [y] (last x)))) {} sums))
(def argval (doall (map (fn [x y] (list x y)) even-fib (rest (zzz even-fib '(0))))))

(defn hhh [n]
  (last (last (take-while (fn [[a b]] (<= a n)) argval))))

(defn solve-me [n]
  (cl-format true "~A~%" (hhh n) ))

;; ---------- functions for reading the inputs ---------------------------------
(defn read-lines
  [f n]
  (map f (repeatedly n read-line)))

(defn as-int
  [x]
  (Integer/parseInt x))

(defn split-line
  [line]
  (.split line " "))

(defn as-ints
  [line]
  (map as-int (split-line line)))

;;; this closure reads the data and sends it to solve-me
(defn solution []
  (let [t (first (read-lines as-int 1))
        ll (read-lines as-int  t)]
    ;; (cl-format true "==== ~A ~A~%" t ll)
    (doall (map (fn [x] (solve-me x)) ll))
    nil))

;; remember to uncomment the following code for hackerrank and lein exec
;; commenting it out can be useful in reloading all the functions
(solution)

;; if you uncomment the above and load-file the file then
;; when clojure repl asks you for Stdin: in the mini buffer you can paste there
;; the content of the input file
