(defun solution (&optional stream)
  (let* ((tests (parse-integer (read-line stream)))
         (data (loop repeat tests
                  collect (parse-integer (read-line stream)))))
    (loop for x in data
       do (format t "~A~%" (if (oddp x)
                               (+ (expt (/ (- x 1)
                                           2)
                                        2)
                                  (/ (- x 1) 2))
                               (expt (/ x
                                        2)
                                     2))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "halloween-party.input.1.txt"))
      (solution s))))

(repl-main)
