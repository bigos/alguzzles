(use '[clojure.string :only (split triml)]
     'clojure.pprint)

(defn factors [n]
  (filter #(zero? (rem n %)) (range 1 (inc n))))

(defn recfactors [n c a])

(defn solve-me [n]
  (cl-format true "==== ~A~%" n))

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
