;;; solution

(def cb ")}]")

(defn mb [c stack]
  (or
   (and (= c \)) (= \( (first stack)))
   (and (= c \}) (= \{ (first stack)))
   (and (= c \]) (= \[ (first stack)))))

(defn balanced? [str stack]

  ;; (println (list "==== " str " === " stack))

  (if (= str '())
    (empty? stack)

    (let [fc (first str)
          c  (some #{fc} cb)
          f  (when c (mb c stack))
          ns (if f
               (rest stack)
               (cons (first str) stack))]

      (recur (rest str) ns))))


;;; Complete the isBalanced function below.
(defn isBalanced [s]

  (if
      (balanced? (map (fn [x] x) s) '())
    "YES"
    "NO")

  )



;;; ----------------------------------------
 (isBalanced "()")

;;; cider-jack-in without project
;;; load this file with FULL path like:
;; (load-file "/home/jacek/Programming/hackerrank/DataStructures/Stacks/balanced-brackets/clojure/solution.clj")
