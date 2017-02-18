;; solution in Clojure
(use 'clojure.java.io 'clojure.pprint)

(defn triangular [n]
  (/ (+ (* n n) n) 2))

(defn fifteenator [n]
  (* 15 (+ (* (+ 1 n) 3) (*
                           (/ (+ (* n n) n) 2)
                           7))))

(defn recursive [n acc l steps]
  (let [ss (if (= steps '()) '(3 2 1 3 1 2 3) steps) ]
    ;; (cl-format true "-- ~A ~A ~A ~A~%" n acc l ss)
    ;; (if (= 7 (count ss))
    ;;   (cl-format true "!!! ~A  zzz ~A~%" acc (/ (/ (- acc 0 ) 5) 3) ))
    ;; fifteenator solves the problem
    ;; calculate to nearest multiple of 15
    ;; (recursive 0 0 60 '()) which has acc value of 810
    ;; then calculate the rest  (recursive 60 810 70 '()) then finish up to 70

    (if (>= n l)
      acc
      (do
        (recur (+ n (first ss))
               (+ n acc)
               l
               (rest ss))))))

(defn solve-me [l]
  (cl-format true "~A~%"
             (if (< l 15)
               (recursive 0 0 l '())
               (let [fm  (- l (mod l 15))
                     acc (fifteenator (- (/ fm 15) 1))]
                 ;; (cl-format true "~A   -- ~A     ~A~%" fm acc (/ fm 15))
                 (recursive fm acc l '())))))

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
