(ns solution)

;;; ========= copy below ============

(defn findMedian [arr]
  (let [sorted (sort arr)
        l      (count sorted)
        i      (quot l 2)]
    (nth sorted i)))

;; (def fptr (get (System/getenv) "OUTPUT_PATH"))

;; (def n (Integer/parseInt (clojure.string/trim (read-line))))

;; (def arr (vec (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))))

;; (def result (findMedian arr))

;; (spit fptr (str result "\n") :append true)

;;; ========= copy above ============

(defn read-data-lines []
  (clojure.string/split (slurp "input0.txt") #"\n"))

(defn -main []
  (let [lines (read-data-lines)
        n (Integer/parseInt (nth lines 0))
        arr (vec (map #(Integer/parseInt %)
                        (clojure.string/split (nth lines 1) #" ")))]
    (println
     (str (findMedian arr)))))
