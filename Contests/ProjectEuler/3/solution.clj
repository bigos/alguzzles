(use '[clojure.string :only (split triml)])

(let [t_t (read-line)
      t (Integer/parseInt t_t)]
 (loop [a0 t]
  (when (> a0 0)
    (let [n_t (read-line)
          n (Integer/parseInt n_t)])
    (recur (- a0 1) ) )))
