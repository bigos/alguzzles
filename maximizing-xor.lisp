(defun solution (&optional stream)
  (let* ((a (parse-integer (read-line stream)))
         (b (parse-integer (read-line stream))))
    ))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "maximizing-xor.input1.txt"))
      (solution s)
      (format t "~&------------~%"))))

(repl-main)
