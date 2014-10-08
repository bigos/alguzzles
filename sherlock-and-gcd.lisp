(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun factor (n &optional (acc '()))
  (when (> n 1) (loop with max-d = (isqrt n)
                   for d = 2 then (if (evenp d) (1+ d) (+ d 2)) do
                     (cond ((> d max-d) (return (cons (list n 1) acc)))
                           ((zerop (rem n d))
                            (return (factor (truncate n d) (if (eq d (caar acc))
                                                               (cons
                                                                (list (caar acc) (1+ (cadar acc)))
                                                                (cdr acc))
                                                               (cons (list d 1) acc)))))))))

(defun primep (n)
  (equalp (car (factor n))
           (list n 1)))

(defun puzzle-2 (n nums)
  (format t "nums are: ~A ~%" nums)
  (if (>= (length nums) 3)
      (cond ((< n 3) nil)
            ((eq n 3) (apply '< nums))
            (T (progn
                 (loop repeat (- (length nums) 2)
                    for x = 0 then (+ x 1)
                    for y = (+ x 3) then (+ y 1)
                    for z = (subseq nums x y)
                    for res = (apply '<= z)
                    do (format t "~A ~%" z)
                    until res
                    finally (return res)))))))

(defun puzzle (data)
  (let ((n (car data))
        (nums (cadr data)))
    ;; (format t "~A ~A~%" n nums)
    (if (puzzle-2 n nums)
        (princ "YES")
        (princ "NO"))
    (terpri)))

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
      (solution s))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "sherlock-and-gcd.input.2.txt"))
      (solution s))
    ))

(repl-main)
