;; Complete the weightedUniformStrings function below.

(def char-weights (zipmap
                   (map (fn [x] (char (+ 96 x))) (range 1 27))
                   (range 1 27)))
(defn freqs [str]
  (frequencies (map (fn [y] (get char-weights y)) str)))

(defn recme [q ff n ne]
  ;; (print "\n\n")
  ;; (print q " >>>>>>>>>>>\n")
  ;; (print ff "\n")
  ;; (print n "\n")
  ;; (print  ne " <<<<<<<<<<<<<<<\n")
  (if (> n ne)
    false
    (let [v (get ff n)
          r (range n (+ 1 (* v n)) n)]
      (if (some (fn [a] (= a q)) r)
        true
        (recur q ff (+ 1 n) ne)))))

(defn qmatch [q ff]
  (recme q ff 1 (last (keys ff))))

;; (weightedUniformStrings "abccddde" [1 3 12 5 9 10])
(defn weightedUniformStrings [s queries]
  (let [ff (freqs s)]
      (map (fn [q]
             (if (qmatch q ff)
               "Yes"
               "No")) queries)))

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
