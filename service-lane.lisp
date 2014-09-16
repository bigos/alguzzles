;;

(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun solution (&optional stream)
  (let* ((first-input) (n) (tests) (width))
    (setf first-input (split-by-one-space (read-line stream)))
    (setf n (parse-integer (elt first-input 0))
          tests (parse-integer (elt first-input 1))
          width (map 'list (lambda (x) (parse-integer x)) (split-by-one-space (read-line stream)))
          )

    (format t "n ~a tests ~a width ~s~%" n tests width)
    ))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if (search "chess" (machine-instance))
                  "Documents/hackerrank/"
                  "Programming/HackerRank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "service-lane.input1.txt"))
      (solution s))))

(repl-main)
