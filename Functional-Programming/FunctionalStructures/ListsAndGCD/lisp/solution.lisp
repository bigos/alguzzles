(defun two-same (l last)
  (eq (car l) last))

(defun compress (l count last acc)
  (if (not l)
      (progn
        (when last
          (push last acc))
        (when (> count 0)
          (push count acc))
        (reverse acc))
      (progn
        (if (two-same l last)
            (progn
              (compress (cdr l) (1+ count) (car l) acc))
            (progn
              (when last
                (push last acc))
              (when (> count 1)          
                (push count acc))
              (compress (cdr l) 1 (car l) acc))))))

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

;; (time (divisors 228175654564568779))
;; Evaluation took:
;; 11.635 seconds of real time
;; 11.624000 seconds of total run time (11.620000 user, 0.004000 system)
;; 99.91% CPU
;; 33,664,519,926 processor cycles
;; 89,408 bytes consed
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

(defun prime-divisors (n)
  (remove-if-not #'primep (divisors n)))

(defun get-base (l res)
  (if (not l)
      res
      (get-base (cddr l) (cons (expt (car l) (cadr l)) res))))

(defun get-result (l)
  (apply #'* (get-base l nil)))

(defun prime-factors (n) n)

;; http://www.mathwarehouse.com/arithmetic/numbers/prime-number/prime-factorization-calculator.php
(defun solve-me (l)
  (let* ((rf (apply #'gcd (map 'list (lambda (x) (get-result x)) l))))
    (princ l)
    ;; rf is
    ;; rf seems to be the cause of timeouts, prime-factors seem to get choked with it
    ;; 2281687790422554067453416082191776817489009509246934422242458148952537553271

    ;; on input 03 we get wrong answer when we decompose prime divisors of 735000
    (loop for x in (compress (decompose rf (prime-divisors rf)) 1 nil nil)
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
