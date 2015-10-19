(defun partitions (n &aux parts)
  (labels ((f (n part)
             (cond
               ((zerop n) (push part parts))
               (t (loop for i from (if part (car part) 1) to n
                     do (f (- n i) (cons i part)))))))
    (f n nil))
  (nreverse parts))

;;; (variations 2 3) => 000 .. 111
(defun variations (n l)
  (let ((buckets
         (make-array l :initial-element 0))
        (limit n)
        (results))
    (labels ((reset-bucket (level)
               (loop for x from 0 below level do
                    (setf (elt buckets x) 0)))
             (varme (level)
               (when (zerop level )
                 (setf results
                       (append results
                               (list (loop for x from 0 below l
                                        collect (elt buckets x))))))
               (incf (elt buckets level))
               (when (>= (elt buckets level) limit)
                 (when (< level (1- l))
                   (varme (1+ level))))
               (reset-bucket level)
               (setf level 0)))
      (loop  while  (every (lambda (x) (< x  n)) buckets) do
           (varme 0)))
    results))

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

(defun comb (m list)
  (let ((result))
    (labels ((comb1 (l c m)
               (when (>= (length l) m)
                 (if (zerop m) (return-from comb1 (push c result)))
                 (comb1 (cdr l) c m)
                 (comb1 (cdr l) (cons (first l) c) (1- m)))))
      (comb1 list nil m))
    result))

;; (comb 3 '(0 1 2 3 4 5))

(defun nth-cost (n price)
  (let ((result (* (+ n 1) price)))
    (format t "~&<<<< ~A ~A === ~A~%" n price result)
    result))

(defun costs1 (costs)
  (format t "entering costs1 ~A~%" costs)
  (loop for c in costs
     for i = 0 then (1+ i)
     for r = (nth-cost i c)
     sum r))

;; CL-USER> (mapped-costs '(1 3) '(2 2 1 1))
;; ((2) (2 1 1))
;; CL-USER> (mapped-costs '(2 2) '(2 2 1 1))
;; ((2 2) (1 1))
(defun mapped-costs (ps ints)
  (loop for s in (split-list-by-list ps ints) collect s))

(defun split-list-by-list (s l &optional res)
  (if (null s)
      (return-from split-list-by-list res)
      (split-list-by-list (cdr s)
                          (subseq l (car s))
                          (concatenate 'list
                                       res
                                       (list (subseq l 0 (car s)))))))

;;; why it fails ints = (2 2 1 1) or (2 1 2 1)
;;; (2 2) + (1 1) = (2*1+2*2 + 1*1+1*2) = 6+3
;;; (2 1) + (2 1) = (2*1+1*2 + 2*1+1*2) = 4+4 !!!!!
;;; need to think of better way of sorting arguments

;;; (solve-me 4 2 '(1000 100 10 1 ))
(defun solve-me (n k ints)
  (format t "==== ~A ~A ~A =====~%" n k ints)
  (let ((klen-partitions
         (loop for f in (partitions n) when (= k (length f)) collect (reverse  f))))
    (format t "~%partitions ~A~%" klen-partitions)
    (format t ">results of permutations>>> ~A~%"
            (loop for ps in klen-partitions
                 collect ; results of different partitions
                 (loop for costs in (mapped-costs ps ints)
                    sum (costs1 costs))))))

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
    (solve-me (car nk) (cadr nk) (sort ints '>))))

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
                                    "input1A.txt"))
      (solution s))))
