(defun solve-me (m n prices)
  (format t "~A ~A ~A~%" m n prices))

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
  (let ((test-cases (parse-integer (read-line stream)))
        (m)
        (n)
        (prices))
    (dotimes (x test-cases)
      (setf m (parse-integer (read-line stream))
            n (parse-integer (read-line stream))
            prices (split-and-parse (read-line stream)))
      (solve-me m n prices))))

 ;; (solution) ; uncomment this when running on hacker-rank

;;; still need to add  removing vertices
(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "Search/lisp/ice-cream-parlor/"
                                    "input0.txt"))
      (solution s))))
