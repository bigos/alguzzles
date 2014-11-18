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
  (let ((first-input) (n) (tests) (width) (test-cases) (result))
    (setf first-input (split-by-one-space (read-line stream))
          n (parse-integer (elt first-input 0))
          tests (parse-integer (elt first-input 1))
          width (split-and-parse (read-line stream))
          test-cases (loop repeat tests
                        collect  (split-and-parse (read-line stream))))

    (format nil "submitted data: n ~a tests ~a width ~s test cases ~S~%" n tests width test-cases)
    (setf result (loop for tc in test-cases
                    collecting
                      (loop for z in (subseq width (car tc) (1+ (cadr tc)))
                         minimize z)))
    (loop for r in result
       do (format t "~&~A~%" r))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "service-lane.input1.txt"))
      (solution s)
      (format t "------------~%"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "service-lane.input2.txt"))
      (solution s)
      (format t "------------~%"))))

(repl-main)
