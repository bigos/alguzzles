;; solution in Clojure
(use 'clojure.java.io 'clojure.pprint)

(defn recursive [n acc l steps]
  (let [ss (if (= steps '()) '(3 2 1 3 1 2 3) steps) ]
    (cl-format true "-- ~A ~A ~A ~A~%" n acc l ss)
    (if (= 7 (count ss))
      (cl-format true "!!! ~A  zzz ~A~%" acc (/ (/ (- acc 0 ) 5) 3) ))
    ;; values of zzz - old-zzz differ by 7
    ;; starting with 3
    ;; previous zzz + zzz + 7 = new zzz
    ;; 3 + 7 + 7 + 7 or
    ;; for all positions except the first
    ;; 7 * pos - 4, so
    ;; 7-4 + 14-4 + 21-4 + 28-4
    ;;  7 * 1 - 4 = 3, 7 * 2 - 4 = 10, 7 * 3 - 4 = 17, 7 * 4 -4 = 23
    ;; 3*3*5 = 45, 13*3*5 = 195, 30*3*5=450
    ;; 9+6=13, 21+9=30, 42+12=54, 70+15=85, 105+18=123, 147+21=168, 196+24=220, 252+27=279
    ;; x+y=z where y=3*a and z is the result in the above line
    ;; sequence (map (fn[x] [x (* x 7)]) (range 1 45)) has above values of x
    ;; where first part is the key and the value becomes x two lines above
    ;;  21 is 3*7, 42 is 6*7, 70 is 10*7, 105 is 15*7, 147 is 21*7, 196 is 28*7, 252 is 36*7
    ;; 1*3*7, 2*3*7, 2*5*7, 3*5*7, 3*7*7, 4*7*7, 4*9*7
    ;; triangular numbers?
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
