(defun solve-me (n k rq cq ii)
  (format t "=== ~A ~A ~A ~A ~A~%" n k rq cq ii))

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
  (let* ((nk (split-and-parse (read-line stream)))
         (n (car nk))
         (k (cadr nk))
         (rqcq (split-and-parse (read-line stream)))
         (rq (car rqcq))
         (cq (cadr rqcq))
         (ii (loop for x from 1 to k collect (split-and-parse (read-line stream)))))
    (solve-me n k rq cq ii)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
