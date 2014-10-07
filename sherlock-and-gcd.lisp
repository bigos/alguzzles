(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun puzzle-2 (n nums)
  (if (>= (length nums) 3)
      (cond ((< n 3) nil)
            ((eq n 3) (apply '< nums))
            (T (progn
                 (loop repeat (- (length nums) 2)
                    for x = 0 then (+ x 1)
                    for y = (+ x 3) then (+ y 1)
                    for z = (subseq nums x y)
                    for res = (apply '< z)
                    do (format nil "~A ~A~%" nums z)
                    until res
                    finally (return res))
                 )))
      ))

(defun puzzle (data)
  (let ((n (car data))
        (nums (cadr data)))
    ;; (format t "~A ~A~%" n nums)
    (if (puzzle-2 n nums)
        (princ "YES")
        (princ "NO"))
    (terpri)
    ))

(defun solution (&optional stream)
  (let* ((tests (parse-integer (read-line stream)))
         (data (loop repeat tests
                  collect (list (parse-integer (read-line stream))
                                (split-and-parse (read-line stream))))))
    (loop for dataset in data
       do (puzzle dataset))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "sherlock-and-gcd.input.1.txt"))
      (solution s))))

(repl-main)
