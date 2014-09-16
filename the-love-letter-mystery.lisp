(defun palindromep (str)
  (equal str (nreverse str)))

(defun char-dist (pair)
  (abs (- (char-code (car pair)) (char-code (cdr pair)))))

(defun str-ends (str x)
  (let ((strlen (length str)))
    (if (> x (floor (/ strlen 2)))
        (cons (elt str (- x 1)) (elt str (- strlen x)))
        (cons (elt str (- strlen x 1)) (elt str x)))))


(defun solution (&optional stream)
  (let* ((tests (parse-integer (read-line stream)))
         (strings (loop repeat tests collect (read-line stream))))
    (loop for str in strings
       collecting
         )

    ))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "the-love-letter-mystery.input1.txt"))
      (solution s)
      (format t "------------~%"))))

(repl-main)
