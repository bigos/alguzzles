(defun solution (&optional stream)
  (let* ((n (parse-integer (read-line stream)))
         (k (parse-integer (read-line stream)))
         (data (make-array (list n)
                           :initial-contents
                           (loop repeat n
                              collect  (parse-integer (read-line stream))))))
    (format t "~a ~a ~a~%" n k data)))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/"))
        (puzzle "angry-children"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input.1.txt"))
      (solution s))))

(repl-main)
