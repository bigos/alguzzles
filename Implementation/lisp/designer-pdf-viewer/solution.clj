(use 'clojure.java.io)

(defn solution
  []
  (println  *in*)
  ;; (doseq [line (line-seq (java.io.BufferedReader *in*))]
  ;;   println line)
  )


 (solution) ; uncomment on Hackerrank



;; (solution (slurp "/home/jacek/Programming/alguzzles/Implementation/lisp/designer-pdf-viewer/input0.txt")
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
