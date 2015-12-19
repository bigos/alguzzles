;; (solve-me '(1 2 3 4 5) '(6 7 8 9 10) '(1 4))
(defun solve-me (as bs lr)
  (if (not as)
      0
      (solve-me (+ (* (car as)
                      (expt STUCK-HERE (car bs))) ;what is x ?
                   (solve-me (cdr as) (cdr bs) lr)))))

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
  (let ((as (split-and-parse (read-line stream)))
        (bs (split-and-parse (read-line stream)))
        (lr (split-and-parse (read-line stream))))
    (format t "read ~A ~A ~S" as bs lr)
    (solve-me as bs lr)
    ))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
