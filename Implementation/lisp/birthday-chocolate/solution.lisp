(defun combsucc (n nc d m)
  (loop for x from 0 to (- n m )
     for ss = (subseq nc x (+ x m))
     when (eq d (apply #'+ ss))
     collect ss))

(defun solve-me (n nc d m)
  (format t "~a" (length (combsucc n nc d m))))

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
  (let ((n (parse-integer (read-line stream)))
        (nc (split-and-parse (read-line stream)))
        (dm (split-and-parse (read-line stream))))
    (destructuring-bind (d m)
        dm
      (solve-me n nc d m))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
