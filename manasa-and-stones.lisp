(defun expotential-divisors (n expot)
  (reverse (loop for x = 1 then (* x expot ) until (> x n) collect x)))

(defun expot-len (n)
  (1- (expt 2 (1- n))))

(defun calc-base (n divs a b)
  (loop for d in divs
     collect (if (zerop (floor (/ n d)))
                     a
                     b)
     do (if (>= n d) (setf n (rem n d)))))

(defun puzzle (n a b)
  (let* ((divs (expotential-divisors n 2))
         (res))
    (loop for x from 0 to (expot-len n)
       do (progn
            (setf res (calc-base x divs a b))
            (format t "~a ~%" (apply '+ res))
            )))
  )

(defun find-values (n a b)
  (format t "arguments: ~a ~a ~a~%" n a b))

(defun solution (&optional stream)
  (let* ((tests (parse-integer (read-line stream)))
         (data (loop repeat tests
                  collect (list (parse-integer (read-line stream))
                                (parse-integer (read-line stream))
                                (parse-integer (read-line stream))))))
    (loop for dataset in data
       do (find-values (nth 0 dataset)
                       (nth 1 dataset)
                       (nth 2 dataset)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "manasa-and-stones.input.1.txt"))
      (solution s))))

(repl-main)
