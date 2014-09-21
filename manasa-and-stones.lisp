(defun solution (&optional stream)
  (let* ((tests (parse-integer (read-line stream)))
         (ncms (loop repeat tests
                  collect (list (read-line stream)
                                (read-line stream)
                                (read-line stream)))))
    (format t "~A~%" ncms)))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "manasa-and-stones.input.1.txt"))
      (solution s))))

(repl-main)
