(defun small-divisors (n fls)
  (loop for d from 1 to fls
     when (zerop (mod n d))
     collect d))

(defun divisors (n)
  (let* ((nsqrt (sqrt n))
         (small-divisors (small-divisors n (floor nsqrt)))
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

(defun find-gcd (da db)
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
                      :name "input0" :type "txt"))
    (solution s)))

(main)
