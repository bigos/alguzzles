(defun solution (&optional stream)
  (let* ((a (read-line stream)))
    (format t "~A" a)
   ))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "game-of-thrones-1.input1.txt"))
      (solution s)
      (format t "~&------------~%"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "game-of-thrones-1.input2.txt"))
      (solution s)
      (format t "~&------------~%"))))

(repl-main)
