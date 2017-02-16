;;
(use 'clojure.java.io 'clojure.pprint)

(defn indexes-hash
  [s]
  (reduce (fn [acc [i ch]]
            (update acc ch conj i))
          {}
          (map-indexed list s)))

(defn counts-hash
  [s]
  (reduce (fn [a x]
            (update a x (fn [y]
                          (+ 1 (get a x 0)))))
          {}
          s))

(defn sort-indexes
  [ii]
  (sort (fn [x y] (if (= (count x) (count y))
                    (> (first x) (first y))
                    (> (count x) (count y))))
        ii))

(defn combinations-wrep
  "Combinations with repetition"
  [l]
  (for [x l
        y l]
    [x y]))

(defn compare-sorted [l]
  (= l
     (sort (fn [x y] (> x y))
           l)))

(defn first-choice [x y f]
  (if (apply f [(first x)
                (first y)])
    x
    y))

(defn foo [x y]
  (let [a (first-choice x y >)
        b (first-choice x y <)
        maxnum 1000000]
    (let [res (flatten (map (fn [x y] [x y]) a b))]
      (if (= (first res) maxnum)
        (rest res)
        res))))

(defn zipme [p]
  (let [a (first p)
        b (last p)
        maxnum 1000000]
    (let [res (cond
                (= (count a) (count b))       (foo a b)
                (= (+ 1 (count a)) (count b)) (foo (cons maxnum a) b)
                (= (count a) (+ 1 (count b))) (foo a (cons maxnum b)))]
      (if (and res (compare-sorted res))
        (count res)
        0))))


(defn solve-me
  [sl s]
  ;; (cl-format true "===== ~A ~S~%" sl s)
  (let [hi (indexes-hash s)
        hc (counts-hash s)
        res (first
             (sort >
                   (map zipme
                        (remove (fn [x] (= (first x) (last x)))
                                (combinations-wrep (sort-indexes (vals hi)))))))]
    (cl-format true "~S~%"   (if res
                               res
                               0))))

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
(defn solution []
  (let [[sl] (read-lines as-ints 1)
        [s] (doall (read-lines identity 1))]
    (solve-me sl s)))

;; remember to uncomment the following code for hackerrank and lein exec
;; commenting it out can be useful in reloading all the functions
(solution)

;; if you uncomment the above and load-file the file then
;; when clojure repl asks you for Stdin: in the mini buffer you can paste there
;; the content of the input file
