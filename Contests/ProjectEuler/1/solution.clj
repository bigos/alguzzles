;; solution in Clojure
(use 'clojure.java.io 'clojure.pprint)

(defn recursive [n acc l steps]
  (let [ss (if (= steps '()) '(3 2 1 3 1 2 3) steps) ]
    ;;(cl-format true "-- ~A ~A ~A ~A~%" n acc l ss)
    (if (= 7 (count ss))
      (cl-format true "!!! ~A  zzz ~A~%" acc (/ (/ (- acc 0 ) 5) 3) ))
    ;; values of zzz - old-zzz differ by 7
    ;; starting with 3
    ;; previous zzz + zzz + 7 = new zzz
    ;; 3 + 7 + 7 + 7 or
    ;; for all positions except the first
    ;; 7 * pos - 4, so
    ;;  7 * 1 - 4 = 3, 7 * 2 - 4 = 10, 7 * 3 - 4 = 17, 7 * 4 -4 = 23
    ;; 3*3*5 = 45, 13*3*5 = 195, 30*3*5=450
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
