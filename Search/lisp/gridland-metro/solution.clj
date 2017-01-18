(use 'clojure.java.io 'clojure.pprint)

(defn solve-me
  [n m k ii]
  (cl-format true "~&solving ~s ~s ~s ~s~%" n m k ii)
  ;; sum each line
  ;; where line is
  ;; subtracting length of (overlapping) tracks from number n
  ;; possibly counting points from range n that are not in track ranges
  ;; read about operations on sets
  )

;; ---------- functions for reading the inputs ---------------------------------
(defn read-lines
  [f n]
  (map f (repeatedly n read-line)))

(defn as-int
  [x]
  (Integer/parseInt x))

(defn split-line
  [line]
  (.split line " "))

(defn as-ints
  [line]
  (map as-int (split-line line)))

;;; this closure reads the data and sends it to solve-me
(let [nmk (first (doall (read-lines as-ints 1)))]
  (let [n (nth nmk 0)
        m (nth nmk 1)
        k (nth nmk 2)]
    (let [ii (doall (read-lines as-ints k))]
      ;; (cl-format true "~s ~s ~s ~s ~%" n m k ii)
      (solve-me n m k ii))))
