(ns solution)

;;; ========= copy below ============

(defn quickSort [arr]
  (sort arr))


(def n (Integer/parseInt (clojure.string/trim (read-line))))
(def arr (vec (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))))

(def result (quickSort arr))

(def fptr (get (System/getenv) "OUTPUT_PATH"))
(spit fptr (clojure.string/join " " result) :append true)
(spit fptr "\n" :append true)

;;; ========= copy above ============

(defn -main []
  (println
   (str (quickSort [5 4 3 2 1]))))
