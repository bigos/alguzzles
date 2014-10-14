(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun squarep (i)
  (zerop (multiple-value-bind (x y)
             (floor (sqrt i))
           (declare (ignore x)) y)))

(defun puzzle (a b)
  (loop for i from a to b counting (squarep i)))

(defun solution (&optional stream)
  (let* ((testcases (parse-integer (read-line stream)))
         (data (make-array (list testcases 2)
                           :initial-contents
                           (loop repeat testcases
                              collect (split-and-parse (read-line stream))))))
    (loop for x from 0 below testcases
       do (format t "~A~%" (puzzle (aref data x 0) (aref data x 1))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/"))
        (puzzle "sherlock-and-squares"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input.1.txt"))
      (solution s))))

(repl-main)
