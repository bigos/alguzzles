;;; this is going to be useful
(defun expotential-divisors (n expot)
  (loop for x = 1 then (* x expot ) until (> x n) collect x))

(defun calc-base (n expot)
  (declare (optimize (debug 3)))
  (let ((divs (reverse (expotential-divisors n expot))))
    (loop for d in divs
       collect (floor (/ n d))
       do (if (>= n d) (setf n (rem n d))))))

(defun pad-min-len (core len)
  (let* ((cl (length core))
         (padl (if (< cl len)
                   (- len cl)
                   0)))
    (concatenate 'list
                 (loop repeat padl collect 0)
                 core)))
;;; todo: fix me
(defun puzzle (a b l)
  (let ((term (expt l 2)))
    (loop for x from 0 to (1- term)
       collect (apply '+
                      (substitute a
                                  0
                                  (substitute b
                                              1
                                              (pad-min-len (calc-base x 2)
                                                           l)))))))

(defun find-me (n r values)
  (format t "~A ~A ~% " n r)
)

(defun find-values (n a b)
  (find-me n 0 (list a b))
  (format t "~%"))

(defun solution (&optional stream)
  (let* ((tests (parse-integer (read-line stream)))
         (data (loop repeat tests
                  collect (list (parse-integer (read-line stream))
                                (parse-integer (read-line stream))
                                (parse-integer (read-line stream))))))
    (format t ">>> ~A~%" data)
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
