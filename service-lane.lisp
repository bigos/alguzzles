;;

(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun solution (&optional stream)
  (let* ((first-input) (n) (tests) (width) (test-cases))
    (setf first-input (split-by-one-space (read-line stream)))
    (setf n (parse-integer (elt first-input 0))
          tests (parse-integer (elt first-input 1))
          width (split-and-parse (read-line stream)))
    (loop repeat tests
       do (push (split-and-parse (read-line stream)) test-cases))
    (setf test-cases (nreverse test-cases))


    (format t "n ~a tests ~a width ~s test cases~S~%" n tests width test-cases)
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
