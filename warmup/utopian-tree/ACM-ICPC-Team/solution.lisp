(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun split-string-to-chars (str)
  (loop for x from 0 to (- (length str) 1)
     collecting (char str x)))

(defun solution (&optional stream)
  (let* ((nm (split-and-parse (read-line stream)))
         (data (loop repeat (car nm)
                  collect (split-string-to-chars (read-line stream)))))
    (format t "~A ~s~%" nm data)))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/"))
        (puzzle "ACM-ICPC-Team"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input.1.txt"))
      (solution s))))

(repl-main)
