;; Complete the missingNumbers function below.

(defn missingNumbers [arr brr] (list "hello"))

(def fptr (get (System/getenv) "OUTPUT_PATH"))

(def n (Integer/parseInt (clojure.string/trim (read-line))))

(def arr (vec (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))))

(def m (Integer/parseInt (clojure.string/trim (read-line))))

(def brr (vec (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))))

(def result (missingNumbers arr brr))

(spit fptr (clojure.string/join " " result) :append true)
(spit fptr "\n" :append true)
