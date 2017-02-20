(use '[clojure.string :only (split triml)]
     'clojure.pprint)

(defn primefactors
  ([n]
   (primefactors n 2 '()))
  ([n candidate acc]
   (cond (<= n 1)  acc
         (zero? (rem n candidate)) (recur (/ n candidate) candidate (cons candidate acc))
         :else (recur n (inc candidate) acc))))

(defn puzzle [n]
  (first (primefactors n)))

(def mempuzzle (memoize puzzle))

(defn solve-me [n]
  (cl-format true "~A~%" (mempuzzle n)))

;;; -----------------------------------------------------------------------------
(defn solution []
  (let [t_t (read-line)
        t (Integer/parseInt t_t)]
    (loop [a0 t]
      (when (> a0 0)
        (let [n_t (read-line)
              n (Integer/parseInt n_t)]
          (solve-me n))
        (recur (- a0 1) ) ))))

(solution)
