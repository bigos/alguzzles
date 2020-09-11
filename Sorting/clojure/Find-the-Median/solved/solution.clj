(ns solution)

;;; ========= copy below ============

(defn findMedian [arr]
  (let [sorted (sort arr)
        l      (count sorted)
        i      (quot l 2)]
    (nth sorted i)))

(def fptr (get (System/getenv) "OUTPUT_PATH"))

(def n (Integer/parseInt (clojure.string/trim (read-line))))

(def arr (vec (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))))

(def result (findMedian arr))

(spit fptr (str result "\n") :append true)

;;; ========= copy above ============

(defn -main []
  (println
   (str (findMedian [0 1 2 4 6 5 3]))))
