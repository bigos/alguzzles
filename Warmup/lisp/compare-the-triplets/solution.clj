(use 'clojure.java.io 'clojure.pprint)

(defn solve-me
  [aa bb]
  (cl-format true "~&solving ~s ~s ~%" aa bb)
  (loop [a aa
         b bb
         ac 0
         bc 0 ]
    (when (> (count a) 0)
      (cl-format true "pair ~S ~S ~s ~s   ~s ~s~%" a b ac bc  (+ a ac) (+ b bc))
      (recur (rest a) (rest b)
             (if (> (first a) (first b)) (+ ac 1) ac)
             (if (< (first a) (first b)) (+ bc 1) bc)))))

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
(let [aa (read-lines as-ints 1)
      bb (read-lines as-ints 1)]
  (solve-me (first aa) (first bb)))
