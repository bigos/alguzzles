;; Complete the missingNumbers function below.

(defn missingNumbers [arr brr]
  (let [ag (frequencies arr)
        bg (frequencies brr)
        an (sort (distinct (concat (keys ag) (keys bg))))]
    (for [n     an
          :let  [ac (get ag n)
                bc (get bg n)]
          :when (not= ac bc)]
      n)))

(def n (Integer/parseInt (clojure.string/trim (read-line))))

(def arr (vec (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))))

(def m (Integer/parseInt (clojure.string/trim (read-line))))

(def brr (vec (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))))

;;; comment is out when submitting
;; (def a1 [203 204 205 206 207 208 203 204 205 206])
;; (def b1 [203 204 204 205 206 207 205 208 203 206 205 206 204])
;; (defn huh []
;;   (missingNumbers a1 b1))

(defn solve []
  (let [fptr (get (System/getenv) "OUTPUT_PATH")
        ;; uncomment when deploying
        result (missingNumbers arr brr)
        ;; result (missingNumbers a1 b1)
        ]

    (spit fptr (clojure.string/join " " result) :append true)
    (spit fptr "\n" :append true)))

(solve)
