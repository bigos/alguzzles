(defun solution (&optional stream)
  (let* ((a (string-to-char (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "cut-the-sticks.input1.txt"))
      (solution s))))

(repl-main)
