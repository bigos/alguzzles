;; solution in Clojure
(use 'clojure.java.io 'clojure.pprint)

(defn recursive [n acc l steps]
  (let [ss (if (= steps '()) '(3 2 1 3 1 2 3) steps) ]
    (if (>= n l)
      acc
      (do
        (recur (+  n (first ss))
               (+ n acc)
               l
               (rest ss))))))

(defn solve-me [l]
  (cl-format true "~A~%"
             (recursive 0 0 l '())))

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
