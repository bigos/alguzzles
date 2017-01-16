(use 'clojure.java.io)

(println "aaaaaaaaaaaaaaaaaaaaaa")
(print (type *in*))

(println "")

(println (line-seq (java.io.BufferedReader. *in*)))
;; (defn solution
;;   []
;;   (println (type *in*))
;;   ;; (doseq [line (line-seq (java.io.BufferedReader *in*))]
;;   ;;   println line)
;;   )


;;  (solution) ; uncomment on Hackerrank



;;
;;  )
;; -----------------------------------------------------------
;; (defn read-lines
;;   [f n]
;;   (map f (repeatedly n read-line)))

;; (defn as-int
;;   [x]
;;   (Integer/parseInt x))

;; (defn split-line
;;   [line]
;;   (.split line " "))

;; (defn as-ints
;;   [line]
;;   (map as-int (split-line line)))

;; (defn char-range [start end]
;;   (map char (range (int start) (inc (int end)))))

;; (let [[heights] (read-lines as-ints 1)
;;       [word] (read-lines identity 1)
;;       hmap (zipmap (char-range \a \z) heights)]
;;   (->> word
;;        (map hmap)
;;        (apply max)
;;        (* (count word))
;;        (println)))
