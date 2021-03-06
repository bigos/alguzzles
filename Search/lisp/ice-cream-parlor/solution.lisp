(defun solve-me (m n prices)
  (let* ((found1) (found2) (wanted))
    ;; (format t "~& ------->> ~A ~A ~A~%" m n prices)
    (loop for x in prices do
         (setf wanted (- m x))
         (when  (position wanted prices)
           (setf found1 (position x prices))
           (setf found2 (position wanted prices :start (1+ found1))))
       until found2)
    (format t "~A ~A~%" (1+ found1) (1+ found2))))

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
                                    "input02.txt"))
      (solution s))))
