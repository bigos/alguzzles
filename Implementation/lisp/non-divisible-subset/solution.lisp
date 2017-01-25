(declaim (optimize (speed 3)))


;;; (comb 3 '(1 2 4 7) #'print)
(defun comb (m list fn) ; without repetition
  (labels ((comb1 (l c m)
             (when (>= (length l) m)
               (if (zerop m) (return-from comb1 (funcall fn c)))
               (comb1 (cdr l) c m)
               (comb1 (cdr l) (cons (first l) c) (1- m)))))
    (comb1 list nil m)))

(defun combinator (cc k) ; with repetition
  (cond ((zerop k) '(()))
        ((not cc) nil)
        (T (append
            (map 'list
                 (lambda (x) (cons (car cc) x))
                 (combinator cc (- k 1)))
            (combinator (cdr cc) k)))))

(defun solve-me (n k lis)
  (let ((buckets (make-hash-table))
        (count 0))
    (loop for i in lis
       do
         (incf (gethash (mod i k) buckets 0)))

    (when (> (gethash 0 buckets 0) 0)
      (incf count))

    (loop
       for i from 1 to (floor (/ k 2))
       for tmp = (max (gethash i buckets 0)
                      (gethash (- k i) buckets 0))
       do
         (if (and (zerop (mod k 2))
                  (eql i (floor (/ k 2)))
                  (> (gethash (floor (/ k 2)) buckets 0) 0))
             (incf count)
             (incf count tmp)))

    (format t "~A~%" count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (aa (split-and-parse (read-line stream))))
    (solve-me (car nk) (cadr nk) aa)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input09" :type "txt"))
    (solution s)))

(main)
