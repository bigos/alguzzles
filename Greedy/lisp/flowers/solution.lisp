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
               (loop for x from 0 below level do (setf (elt buckets x) 0)))
             (variation (level)
               ;; get value and append it to results
               (when (zerop level )
                 (setf results
                       (append results
                               (list (loop for x from 0 below l
                                        collect (elt buckets x))))))
               ;; increase value
               (incf (elt buckets level))
               ;; go to next level if necessary
               (when (>= (elt buckets level) limit)
                 (when (< level (1- l))
                   (variation (1+ level))))
               ;; zero lower levels
               (reset-bucket level)
               (setf level 0)))
      (loop  while  (every (lambda (x) (< x  n)) buckets) do
           (variation 0)))
    results))

;; https://en.wikipedia.org/wiki/Partition_%28number_theory%29
;; (adds-to 2 9 '(1 2 3 4 5 6 7 8)) => ((8 1) (7 2) (6 3) (5 4))
(defun adds-to (n sums nums)
  (loop for x in (comb n nums)
     when (eq sums (apply '+ x)) collect x))

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
    ;; (format t " << (~A+1) * ~A = ~A << " n price result)
    result))

;;; big surprise
;;; both pairs of inputs are equal
;; CL-USER> (costs1 '( 100000000 1))
;; CL-USER> (costs1 '( 10000 100))
;; ----------------------------------
;; CL-USER> (costs1 '( 100000000 100))
;; CL-USER> (costs1 '( 10000 1))
(defun costs1 (costs)
  (loop for c in costs
     for i = 0 then (1+ i)
     for r = (nth-cost i c)
     sum r))

;;; we don't need mapped costs, we will do something like nn nn n of sorted values
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


(defun my-sort-values (n k ints)     ; almost there, but still no good
  (declare (ignore n))
  (let ((karr (make-array k :initial-element '()))
        (sorted-ints (sort ints '<)))
    (loop for i in sorted-ints
       for n = 0 then (1+ n)
       for x = (mod n k) do
       ;; (format t  "~&going to push ~A ~A to ~A = ~A~%" i n x (elt karr x))
         (push i (elt karr x)))
    ;;(princ karr)
    (loop for z from 0 below k
       sum (costs1 (elt karr z)))))

;;; why it fails ints = (2 2 1 1) or (2 1 2 1)
;;; (2 2) + (1 1) = (2*1+2*2 + 1*1+1*2) = 6+3
;;; (2 1) + (2 1) = (2*1+1*2 + 2*1+1*2) = 4+4 !!!!!
;;; need to think of better way of sorting arguments

(defun find-solution (n k ints)
  ;; (format T "~A ~A ~A~%" n k ints)
  (let ((klen-partitions
         (loop for f in (partitions n) when (= k (length f))
            collect (reverse  f))))
    (loop for p in (permute ints)
       minimize
         (solve-me n k p klen-partitions))))

;;; (solve-me 4 2 '(1000 100 10 1 ))
(defun solve-me (n k ints klen-partitions)
  (declare (ignore n k))
  (let ((res))
    (setf res
          (loop for ps in klen-partitions
             for mapped-costs = (mapped-costs ps ints)
             do (format nil "------ ~A~%" ps)
             minimize                ; results of different partitions
               (loop for costs in mapped-costs
                  for ccc = (costs1 costs)
                  do (format nil "~&~A ~A~%" costs ccc)
                  sum ccc)))
    res))

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
    (princ (my-sort-values (car nk) (cadr nk) ints))))

 ;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input1A" :type "txt"))
    (solution s)))
;;; main runs upon (load the-file)
(main)
