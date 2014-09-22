(defparameter dat '(a b c))

(defun loopy (l d)
  (let ((ld (length d)))
    (loop for x in d
       collect (list x (if (> l 0)
                           (loopy (1- l) d))))))

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
