;; Complete the weightedUniformStrings function below.

(def char-weights (zipmap
                   (map (fn [x] (char (+ 96 x))) (range 1 27))
                   (range 1 27)))

(defn weigth-nums2 [s]
  (map (fn [x]
         (let [firnum (get char-weights (first x))]
           [firnum
            (range firnum (+ 1 (* firnum (count x))) firnum)]))
       (vals (group-by identity s))))

;; (weightedUniformStrings "abccddde" [1 3 12 5 9 10])
(defn weightedUniformStrings [s queries]
  (let [scores (flatten (map (fn [a] (first (rest a)) ) (weigth-nums2 s)))]
    (map (fn [q]  (if (some #(= % q) scores) "Yes" "No")) queries)))

(def fptr (get (System/getenv) "OUTPUT_PATH"))

(def s (read-line))

(def queries-count (Integer/parseInt (clojure.string/trim (read-line))))

(def queries [])

(doseq [_ (range queries-count)]
  (def queries (conj queries (Integer/parseInt (read-line))))
  )

(def result (weightedUniformStrings s queries))

(spit fptr (clojure.string/join "\n" result) :append true)
(spit fptr "\n" :append true)
