(defun two-same (l last)
  (eq (car l) last))

;; (compress (str-to-list "aabbbccccdeff-") 1 nil)
(defun compress (str count last acc)
  (if (not str)
      (progn
        (when last
          (push last acc))
        (when (> count 0)
          (push count acc))
        (reverse acc))
      (progn
        (if (two-same str last)
            (progn
              (compress (cdr str) (1+ count) (car str) acc))
            (progn
              (when last
                (push last acc))
              (when (> count 1)          
                (push count acc))
              (compress (cdr str) 1 (car str) acc)
              )))))

(defun decompose-inner (n prime-factors acc)
  (let ((res (/ n (car prime-factors))))
  ;; (format t "~a >>> ~A   ~A : ~A~%" res n prime-factors acc)
    (if (eq res 1)
        (reverse  acc)    
        (if (eq (type-of res) 'RATIO)
            (decompose-inner n (cdr prime-factors) (cons (cadr prime-factors) acc))
            (decompose-inner res prime-factors (cons (car prime-factors) acc))))))

(defun decompose (n prime-factors)
  (if (eq 1 (length prime-factors))
      (decompose-inner n prime-factors prime-factors)
      (decompose-inner n prime-factors nil)))

(defun small-divisors (n f)
  "divisors of n from 1 to f, which is (floor (sqrt n))"   
  (loop for x from 1 to f
     when (zerop (mod n x))
     collect x))

(defun large-divisors (n small-divisors)
  "divisors from (sqrt n) to n"
  (loop for x in (reverse small-divisors)
     collect (/ n x)))

(defun divisors (n)
  (let ((small-divs))
    (multiple-value-bind (f r) (floor (sqrt n))
      (setf small-divs (small-divisors n f))
      (concatenate
       'list
       small-divs
       (if (zerop r)
           (cdr (large-divisors n small-divs))
           (large-divisors n small-divs))))))

(defun primep (n)
  (equalp (divisors n)
          (list 1 n)))

(defun prime-factors (n)
  (remove-if-not #'primep (divisors n)))

;;; you can use (gcd 6 12 15)

;;; mutually recursive with get-power
(defun get-base (l res)
  (if (not l)
      res
      (get-power (cdr l) (car l) res )))

;;; mutually recursive with get-base
(defun get-power (l base res)
  (get-base (cdr l) (push (expt base (car l)) res)))

(defun get-result (l)
  (let ((number (apply #'* (get-base l nil))))
    ;; (format t "==== ~A ~A~%" l number)
    number))

;; http://www.mathwarehouse.com/arithmetic/numbers/prime-number/prime-factorization-calculator.php
(defun solve-me (l)
  (let* ((results (map 'list (lambda (x) (get-result x)) l))
         (rf (apply #'gcd results)))
    ;; basically we need prime factors of greates common divisor
    ;; (format t "~A ~A   ~A   -->-  ~A~%" l results rf (prime-factors rf))
    (loop for x in (compress (decompose rf (prime-factors rf)) 1 nil nil)
       for s = "" then " "
       do
         (format t "~A~A" s x))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((tc (parse-integer (read-line stream))))
    (solve-me (loop for x from 0 below tc
                 collect (split-and-parse (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input1" :type "txt"))
    (solution s)))

(main)
