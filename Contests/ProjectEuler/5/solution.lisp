(defun divisors (n)
  (loop for d from 1 to (ceiling (/ n 2))
        when (zerop (mod n d)) collect d))

(defun subsequent-numbers (nl &optional acc)
  (if (and (cdr nl)
           (eq (1+ (car nl))
               (cadr nl)))
      (subsequent-numbers (cdr nl) (push (car nl) acc))
      (cons (car nl)
            acc)))

(defun almost-subsequent (nl &optional acc missing)
  (cond ((and (cdr nl)
              (eq (1+ (car nl))
                  (cadr nl)))
         (almost-subsequent (cdr nl) (push (car nl) acc) missing))
        ((and (cdr nl)
              (null  missing)
              (eq (+ 2 (car nl))
                  (cadr nl)))
         (almost-subsequent (cdr nl) (push (car nl) acc) (1+ (car nl))))
        (t (list 'missing missing (reverse (cons (car nl)
                                                 acc))))))
(defun zzz (ns)
  (let* ((nn  (apply '* ns))
         (nnd (divisors nn)))
      (list 'divisors nn nnd 'almost (almost-subsequent nnd))    ))

(defun seek (n)
  (loop for x from 1 to (expt 10 4)
     for zzz = (length (subsequent-numbers (divisors x)))
     until (eq zzz n)
     finally (return x)))

(defun solve-me (n)
  (format t "~&~a" (seek n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (loop for x from 1 to tc do (solve-me (parse-integer (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
