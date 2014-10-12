(defun solution (&optional stream)
  (let* ((a (parse-integer (read-line stream)))
         (b (parse-integer (read-line stream))))
    (format t "~A"
            (loop for x from a to (1- b) maximize
                 (loop for y from (1+ x) to b
                    maximize (boole boole-xor x y))))))

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
