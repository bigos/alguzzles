(defun factor (n &optional (acc))
  (when (> n 1)
    (loop with max-d = (isqrt n)
       for d = 2 then (if (evenp d) (1+ d) (+ d 2)) do
         (cond ((> d max-d)
                (if (eq n (caar acc))
                    (progn
                      (incf (cadar acc))
                      (return acc))
                  (return (cons (list n 1) acc))))
               ((zerop (rem n d))
                (return (factor (truncate n d) (if (eq d (caar acc))
                                                   (cons
                                                    (list (caar acc) (1+ (cadar acc)))
                                                    (cdr acc))
                                                   (cons (list d 1) acc)))))))))

(defun find-gcd-components (da db)
  (let ((mfdb (factor db)))
    (loop for r in
         (loop for fa in (factor da)
            collect
              (loop for fb in mfdb
                 when (eq (car fa)
                          (car fb))
                 collect (list (car fa)
                               (min  (cadr fa)
                                     (cadr fb)))))
       collect (car r))))

(defun find-gcd (da db)
  (loop for v in (find-gcd-components da db)
     for r = (apply 'expt v) then (* r (apply 'expt v))
       finally (return r)))

(defun solve-me (n na m mb)
  (declare (ignore n m))
  (mod (find-gcd (apply '* na)
                 (apply '* mb))
       (+ (expt 10 9) 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;qq
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
