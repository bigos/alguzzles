(use 'clojure.java.io 'clojure.pprint, 'clojure.core)

(defn solve-me
  [aa bb]
  ;; (cl-format true "~&solving ~s ~s ~%" aa bb)
  (let [pairs
        (mapv (fn [x y] (cond (> x y) (list 1 0)
                              (< x y) (list 0 1)
                              :else (list 0 0))) aa bb)]
    (cl-format true "~A ~A~%"
               (reduce + (map (fn [a] (first a)) pairs))
               (reduce + (map (fn [b] (first (rest b))) pairs)))))

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
