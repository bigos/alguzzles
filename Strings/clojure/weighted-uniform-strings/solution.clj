;;; solution

(def inputsabc "abccddde")
(def inputs123 [1 3 12 5 9 10])
;;; end of development code

(defn mults [n nn acc]
  ;; (print n nn acc "\n")
  (if (empty? nn)
    acc
    (recur n (rest nn) (cons (+ n (first acc)) acc))))

(defn group-same [seq last sames acc]
  ;; (print seq last sames " ---- " acc "\n")
  (if (= (first seq) :end)
    (rest (reverse  acc))
    (let [nv (if (empty? seq)
               (list :end)
               (rest seq))]
      (if (= last (first seq))
        (recur nv
               (first seq)
               (cons (first seq) sames)
               acc)
        (recur nv
               (first seq)
               []
               (if (empty? sames)
                 (concat (list last) acc)
                 (concat (mults last sames [last]) acc)))))))

(def weights (zipmap (map #(char (+ % 96))
                           (range 1 27))
                     (range 1 27)))

;; REPL (weightedUniformStrings inputsabc inputs123)
;; Complete the weightedUniformStrings function below.
(defn weightedUniformStrings [s queries]
  (let [letter-weights (set (group-same
                             (map (fn [c] (get weights c) ) s)
                             nil [] []))]
    (map (fn [q] (if (contains? letter-weights q) "Yes" "No")) queries)))


(def s (read-line))

(def queries-count (Integer/parseInt (clojure.string/trim (read-line))))

(def queries [])

(doseq [_ (range queries-count)]
  (def queries (conj queries (Integer/parseInt (read-line)))))

(def result (weightedUniformStrings s queries))

(def fptr (get (System/getenv) "OUTPUT_PATH"))
(spit fptr (clojure.string/join "\n" result) :append true)
(spit fptr "\n" :append true)
