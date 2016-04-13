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

(defun get-factors (factor-list)
  (let ((ht (make-hash-table)))
    (loop for e in factor-list do
         (setf (gethash (car e) ht) (car e))
         (setf (gethash (cadr e) ht) (cadr e)))
    ht))

(defun divisors (n)
  (let* ((nsqrt (sqrt n))
         (small-divisors (factor n))
         (reversed-divisors))
    ;; do not include sqrt twice
    (setf reversed-divisors             ;we do the assignment only once, so it's not cheating
          (if (eq (floor nsqrt)
                  (ceiling nsqrt))
              (cdr (reverse small-divisors))
              (reverse small-divisors)))
    (if (= n 1)
        '(1)
        (concatenate 'list
                     small-divisors
                     (map 'list (lambda (x) (/ n x)) reversed-divisors)))))

;;; a bit faster than divisors, but still not good enough
(defun factors (n &aux (lows '()) (highs '()))
  (do ((limit (1+ (isqrt n))) (factor 1 (1+ factor)))
      ((= factor limit)
       (when (= n (* limit limit))
         (push limit highs))
       (remove-duplicates (nreconc lows highs)))
    (multiple-value-bind (quotient remainder) (floor n factor)
      (when (zerop remainder)
        (push factor lows)
        (push quotient highs)))))

;;; timeout on args 154742504910672534362390528 150094635296999121
;;; first big number chokes small-divisors
(defun find-gcd (da db)
  (format t "~A ~A~%" da db )
  (let ((db-hash (make-hash-table))
        (dad (divisors da)))
    (loop for d in (divisors db) do
         (setf (gethash d db-hash) d))
    (loop for d in dad
       for result = 1 then (if (eq d (gethash d db-hash))
                               d
                               result)
       finally (return result))))

(defun solve-me (n na m mb)
  (declare (ignore n m))
  (find-gcd (apply '* na)
            (apply '* mb)))

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
  (let ((n (parse-integer (read-line stream)))
        (na (split-and-parse (read-line stream)))
        (m (parse-integer (read-line stream)))
        (mb (split-and-parse (read-line stream))))
    (format t "~a" (solve-me n na m mb))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input1" :type "txt"))
    (solution s)))

(main)
