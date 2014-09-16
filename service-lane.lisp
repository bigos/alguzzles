;;

(defun solution (&optional stream)
  (format t "~a~%" (read-line stream)))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if (search "chess" (machine-instance))
                  "Documents/hackerrank/"
                  "Programming/HackerRank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "service-lane.input1.txt"))
      (solution s))))

(repl-main)
