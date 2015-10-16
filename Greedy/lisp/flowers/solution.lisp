(defun solve-me (n k ints)
  (format t "~A ~A ~A" n k ints))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((nk (split-and-parse (read-line stream)))
        (ints (split-and-parse (read-line stream))))
    (solve-me (car nk) (cadr nk) ints)))

 ;; (solution) ; uncomment this when running on hacker-rank

;;; still need to add  removing vertices
(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "Greedy/lisp/flowers/"
                                    "input0.txt"))
      (solution s))))
