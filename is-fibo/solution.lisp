(defun fibonaccip (i)
  (oddp i))

(defun solution (&optional stream)
  (let* ((testcases (parse-integer (read-line stream)))
         (data (make-array (list testcases 1)
                           :initial-contents
                           (loop repeat testcases
                              collect (list (parse-integer (read-line stream)))))))
    (format t "~S ~S~%" testcases data)
    (loop for x from 0 below testcases
       do
         (format t
                 "~a~%"
                 (if (fibonaccip x)
                     "IsFibo"
                     "IsNotFibo")))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/"))
        (puzzle "is-fibo"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input.1.txt"))
      (solution s))))

(repl-main)
