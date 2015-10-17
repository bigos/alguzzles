(defun partitions (n &aux parts)
  (labels ((f (n part)
             (cond
               ((zerop n) (push part parts))
               (t (loop for i from (if part (car part) 1) to n
                     do (f (- n i) (cons i part)))))))
    (f n nil))
  (nreverse parts))

;; https://en.wikipedia.org/wiki/Partition_%28number_theory%29
(defun adds-to (n sums nums)
  (let ((collection))
    (comb n nums (lambda (x) (when (eq sums (apply '+ x))
                               (push x collection))))
    collection))

;;; w/o repetition
(defun permute (list)
  (if list
      (mapcan #'(lambda (x)
                  (mapcar #'(lambda (y) (cons x y))
                          (permute (remove x list))))
              list)
      '(()))) ; else

;; (print (permute '(A B Z)))

(defun comb (m list fn)
  (labels ((comb1 (l c m)
             (when (>= (length l) m)
               (if (zerop m) (return-from comb1 (funcall fn c)))
               (comb1 (cdr l) c m)
               (comb1 (cdr l) (cons (first l) c) (1- m)))))
    (comb1 list nil m)))

;; (comb 3 '(0 1 2 3 4 5) #'print)

(defun nth-cost (n price) (* (+ n 1) price))

(defun split-list-by-list (s l &optional res)
  (if (null s)
      (return-from split-list-by-list res)
      (split-list-by-list (cdr s)
                          (subseq l (car s))
                          (concatenate 'list res (list (subseq l 0 (car s)))))))

(defun solve-me (n k ints)
  (format t "~A ~A ~A~%" n k ints)
  (let ((klen-partitions
         (loop for f in (partitions n) when (= k (length f)) collect f)))
    (format t "partitions ~A ~A~%" klen-partitions
            (loop for ps in klen-partitions do
                 (format t "****************** ~A~%" ps)
                 minimize
                 (loop for costs in
                      (loop for s in (split-list-by-list ps ints)
                         collect (sort s '>))
                    ;; do (format t "ccc ~A~%" costs)
                    summing
                      (loop for c in costs
                         for i = 0 then (1+ i)
                         do
                           (format t "=== ~A ~A >> ~A~%" c i (nth-cost i c))
                         summing (nth-cost i c)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((nk (split-and-parse (read-line stream)))
        (ints (split-and-parse (read-line stream))))
    (solve-me (car nk) (cadr nk) (sort ints '<))))

 ;; (solution) ; uncomment this when running on hacker-rank

;;; still need to add  removing vertices
(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "Greedy/lisp/flowers/"
                                    "input1.txt"))
      (solution s))))