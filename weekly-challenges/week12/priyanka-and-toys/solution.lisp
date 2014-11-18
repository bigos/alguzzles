(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))


(defun solution (&optional stream)
  (let* ((n (parse-integer (read-line stream)))
         (w (split-and-parse (read-line stream))))
    (format T "~a~%~A~%" n w)))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/"))
        (puzzle "weekly-challenges/week12/priyanka-and-toys/"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "input.1.txt"))
      (solution s))))

(repl-main)
