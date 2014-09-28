(defun find-values (n a b)
  (format t "arguments: ~a ~a ~a~%" n a b))

(defun solution (&optional stream)
  (let* ((tests (parse-integer (read-line stream)))
         (data (loop repeat tests
                  collect (list (parse-integer (read-line stream))
                                (parse-integer (read-line stream))
                                (parse-integer (read-line stream))))))
    (loop for dataset in data
       do (find-values (nth 0 dataset)
                       (nth 1 dataset)
                       (nth 2 dataset)))))

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
