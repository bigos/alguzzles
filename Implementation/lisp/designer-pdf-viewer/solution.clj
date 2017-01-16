(use 'clojure.java.io 'clojure.pprint)

(defn solve-me
  [heights word]
  (let [[chars] (doall (map (fn [x]  x) word)) ;; stuck here
        ]
    (cl-format true "==== ~A~%" (type chars))
    (cl-format true "==== ~A~%" chars)
      ))

;; -----------------------------------------------------------
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

(let [[heights] (read-lines as-ints 1)
      [word] (doall (read-lines identity 1))]
  (println (list heights "\n" word "\n" ))
  (solve-me heights word ))
