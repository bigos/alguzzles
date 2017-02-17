;; new puzzle https://www.hackerrank.com/challenges/sherlock-and-valid-string

(use 'clojure.java.io
     'clojure.pprint)

(defn solve-me [s]
  (let [ff   (frequencies s)
        gff  (group-by (fn [x] (val x)) ff)
        dgff (into {}
                   (for [[k v] gff] [k (count v)]))
        cc   (vals ff)
        dc   (distinct cc)
        cdc  (count dc)]
    ;;(cl-format true "------------ ~S   ~S  ~S ~s ~S ~S~%" s cc dc ff gff dgff)
    (cl-format true "~A~%"
               (if (cond (= cdc 1)                                    true
                         (> cdc 2)                                    false
                         (and (= cdc 2)
                              (or (= 1 (first (vals dgff)))
                                  (= 1 (last (vals dgff))))
                              ) true
                         :else                                        false)
                 "YES"
                 "NO"))))

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
  (let [[s] (doall (read-lines identity 1))]
    (solve-me s)))

;; remember to uncomment the following code for hackerrank and lein exec
;; commenting it out can be useful in reloading all the functions

(solution)

;; if you uncomment the above and load-file the file then
;; when clojure repl asks you for Stdin: in the mini buffer you can paste there
;; the content of the input file
