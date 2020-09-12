(ns solution)

;;; ========= copy below ============

;; Complete the separateNumbers function below.
(defn separateNumbers [s]

  ;; solve me
  ;; https://www.hackerrank.com/challenges/separate-the-numbers/problem

  )

(def q (Integer/parseInt (clojure.string/trim (read-line))))

(doseq [q-itr (range q)]
  (def s (read-line))

  (separateNumbers s)
  )

;;; ========= copy above ============

(defn read-data-lines []
  (clojure.string/split (slurp "input0.txt") #"\n"))

(defn -main []
  (let [lines (read-data-lines)
        n     (Integer/parseInt (nth lines 0))
        arr   (vec (map
                    #(Integer/parseInt %)
                    (clojure.string/split (nth lines 1) #" ")))]
    (println
     (str (findMedian arr)))))
