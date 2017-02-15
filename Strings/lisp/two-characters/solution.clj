;;
(use 'clojure.java.io 'clojure.pprint)

(defn indexes-hash
  [s]
  (reduce (fn [acc [i ch]]
            (update acc ch conj i))
          {}
          (map-indexed list s)))

(defn counts-hash
  [s]
  (reduce (fn [a x]
            (update a x (fn [y]
                          (+ 1 (get a x 0)))))
          {}
          s))

(defn sort-indexes
  [ii]
  (sort (fn [x y] (if (= (count x) (count y))
                    (> (first x) (first y))
                    (> (count x) (count y))))
        ii))

(defn combinations-wrep
  "Combinations with repetition"
  [l]
  (for [x l
        y l]
    [x y]))

(defn solve-me
  [sl s]
  (cl-format true "===== ~A ~S~%" sl s)
  (let [hi (indexes-hash s)
        hc (counts-hash s)]
    (cl-format true "---- ~s~%" hi)
    (cl-format true "---- ~s~%" hc)
    (cl-format true "---- ~s~%" (sort-indexes (vals hi)))

    (cl-format true "---- ~s~%"
               (remove (fn [x] (= (first x) (last x)))
                       (combinations-wrep (sort-indexes (vals hi)))))))

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
(let [[sl] (read-lines as-ints 1)
      [s] (doall (read-lines identity 1))]
  ;; (println (list heights "\n" word "\n" ))
  (solve-me sl s))
