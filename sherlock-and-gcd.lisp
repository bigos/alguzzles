(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun small-divisors (n)
  (loop for x from 1 to (isqrt n)
     if (zerop (mod n x))
     collect x))

(defun divisors (n)
  (let ((big-divs) (isqrtn (isqrt n)))
    (loop for sd in (small-divisors n)
       for res = (floor n sd)
       do
         (unless (= res isqrtn)
           (push res big-divs)))
    (cdr (append (small-divisors n) big-divs))))

(defun primep (n)
  (= (length (divisors n))
     1))

(defun flatten (structure)
  ;;TODO doesn't work with '((1 . :a) (2 . :b))
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defun test-divisors (nums)
  (let* ((all-divs (flatten (loop for l in nums collect (divisors l))))
         (unique-divs (remove-duplicates all-divs))
         (counted-divs (loop for ud in unique-divs
                          collecting (count ud all-divs))))
    ;; (format t "~a ~a ~a~%" all-divs unique-divs counted-divs)
    (every (lambda (x) (< x (length nums)))
           counted-divs)))

(defun puzzle-2 (n nums)
 (test-divisors nums))

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

    ;; (with-open-file (s (concatenate 'string
    ;;                                 (directory-namestring (user-homedir-pathname))
    ;;                                 path
    ;;                                 "sherlock-and-gcd.input.1.txt"))
    ;;   (solution s))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "sherlock-and-gcd-test-cases/input00.txt"))
      (solution s))
    (format t "------------------------~%")
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "sherlock-and-gcd-test-cases/input11.txt"))
      (solution s))
    ;; both should be yes
    (test-divisors '(66694 14259 1710 57758 10303 28775 10037 7706 88458 76927 73054 56600 92544 7414))
    (test-divisors '(4679 47454 36018 28542 21813 94535 41021 32041 45985 29580 82823))
    ))

(repl-main)
