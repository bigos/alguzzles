(defun two-same (l last)
  (eq (car l) last))

(defun compress (l count last acc)
  (if (not l)
      (progn
        (when last
          (push last acc))
        (when (> count 0)
          (push count acc))
        (cdr (reverse acc)))
      (progn
        (if (two-same l last)
            (progn
              (compress (cdr l) (1+ count) (car l) acc))
            (progn
              (when last
                (push last acc))
              (when (> count 0)          
                (push count acc))
              (compress (cdr l) 1 (car l) acc))))))

(defun get-base (l res)
  (if (not l)
      res
      (get-base (cddr l) (cons (expt (car l) (cadr l)) res))))

(defun get-result (l)
  (apply #'* (get-base l nil)))

(defun prime-factors (n)
  (prime-factors-inner n 2))

(defun prime-factors-inner (n p)
  (cond ((= n 1) nil)
        ((zerop (rem n p))
         (cons p (prime-factors-inner (/ n p) p)))
        (T (prime-factors-inner n (1+ p)))))

;; http://www.mathwarehouse.com/arithmetic/numbers/prime-number/prime-factorization-calculator.php
(defun solve-me (l)
  (let* ((rf (apply #'gcd (map 'list (lambda (x) (get-result x)) l))))
    ;; (princ l)        
    (loop for x in (compress (prime-factors rf) 1 nil nil)
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
                      :name "input03" :type "txt"))
    (solution s)))

(main)
