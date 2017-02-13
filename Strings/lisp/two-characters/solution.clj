;;
(use 'clojure.java.io 'clojure.pprint)

(defn solve-me
  [sl s]
  (cl-format true "===== ~A ~S~%" sl s)
  (let [h {}]
    (cl-format true "---- ~s~%" h)

    (cl-format true "---- ~s~%"
               (map (fn [x y] (assoc h x (cons y (get h x))) )
                    s
                    (range 0 (.length s))))
    (cl-format true "---- ~s~%" h)
    ))

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
