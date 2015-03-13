(defun fibonaccip (i)
  (if (zerop i)
      T
      (= i
         (loop
            for y = 0 then x
            and x = 1 then (+ x y)
            until (>= x i)
            finally (return x)))))

(defun solution (&optional stream)
  (let* ((testcases (parse-integer (read-line stream)))
         (data (make-array (list testcases )
                           :initial-contents
                           (loop repeat testcases
                              collect  (parse-integer (read-line stream))))))
    (loop for x from 0 below testcases
       do
         (format t
                 "~a~%"
                 (if (fibonaccip (aref data x))
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
