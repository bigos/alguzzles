                                        ; Complete the weightedUniformStrings function below.
(defn weightedUniformStrings [s queries]


  )

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
