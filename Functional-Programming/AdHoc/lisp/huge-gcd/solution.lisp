(defun factor (n &optional (acc '()))
  (when (> n 1)
    (loop with max-d = (isqrt n)
       for d = 2 then (if (evenp d) (1+ d) (+ d 2)) do
         (cond ((> d max-d) (return (cons (list n 1) acc)))
               ((zerop (rem n d))
                (return (factor (truncate n d) (if (eq d (caar acc))
                                                   (cons
                                                    (list (caar acc) (1+ (cadar acc)))
                                                    (cdr acc))
                                                   (cons (list d 1) acc)))))))))

(defun prime-factors (n)
  (remove-duplicates
   (sort
    (loop for f in (factor n)
       collect (car f) collect (cadr f))
    '<)))

;; (defun find-divisor (n prime-factors sqr)
;;   (let ((acc))
;;     (loop for a in prime-factors do
;;          (loop for b in prime-factors do
;;               (when (and (< (* a b) sqr)
;;                          (zerop (rem n (* a b))))
;;                 (push (* a b) acc))
;;             until (> b sqr)
;;               )
;;        until (> a sqr))
;;     acc))

(defun find-divisor (n x sqr acc)
  (loop for p in acc ))
;;; i need to figure out how to do it using recursive tree
(defun small-divisors (n)
  (remove-duplicates (sort (find-divisor n 1 (isqrt n) '(prime-factors n))
                           '<)))

;; (factor 9114)
;; ((31 1) (7 2) (3 1) (2 1))
;; prime factors to small divisors
;; (list (* 1 1) (* 2 1) (* 3 1) (* 3 2) (* 7 1) (* 7 2) (* 7 3)
;; (* 31 1) (* 7 3 2) (* 7 7) (* 31 2) (* 31 3) )
;; (1 2 3 6 7 14 21 31 42 49 62 93)
(defun small-divisors-old (n)
  "still too slow"
  (loop for d from 1 to (isqrt n)
     when (zerop (mod n d))
     collect d))

(defun divisors (n)
  (let* ((nsqrt (sqrt n))
         (small-divisors (small-divisors n))
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

;;; timeout on args 154742504910672534362390528 150094635296999121
(defun find-gcd (da db)
  (format t "divisors of ~A ~A~%" da db)
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
  (mod (find-gcd (apply '* na)
                 (apply '* mb))
       (+ (expt 10 9) 7)))

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
                      :name "input0" :type "txt"))
    (solution s)))

(main)
