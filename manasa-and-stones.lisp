;;; this is going to be useful
(defun expotential-divisors (n expot)
  (loop for x = 1 then (* x expot ) until (> x n) collect x))

(defun calculi (n expot)
  (declare (optimize (debug 3)))
  (let ((divs (reverse (expotential-divisors n expot)))
        (n1 n) (d) (r))
    (loop
       while divs
       do
         (progn
           (setf d (floor (/ n1 (car divs))))
           (setf r (if T
                       (rem n1 (car divs))))
           (format t "~A ~A ~A~%" n1 divs r)
           (setf divs (cdr divs))
           (setf n1 d))
       collect d)))

(defun puzzle (a b)
  (let ((term (expt 2 2)))
    (loop for x from 0 to (1- term)
       collect (format nil "~2,'0b" x)
         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-me (n r values)
  (format t "~A ~A ~% " n r)
  (loop for v in values
     do (progn (if (> n 0)
                   (find-me (1- n)  (+ r v)  values))
               )))

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
