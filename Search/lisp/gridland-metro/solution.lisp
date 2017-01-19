 (declaim (optimize (debug 3)))
;;; (declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(defun remove-overlapping (ranges)
  (remove-overlapping-2 (sort ranges
                              (lambda (x y)
                                (<= (the fixnum (car x))
                                    (the fixnum (car y)))))
                        nil
                        0))

(defun remove-overlapping-2 (ranges acc sumacc)
  (format t "KKK ~A ~A ~A~%" ranges acc sumacc)
  (let ((a1 (caar ranges))
        (a2 (cadar ranges))
        (b1 (caadr ranges))
        (b2 (cadadr ranges)))
    (declare (type fixnum a1 a2 b1 b2))
    (cond ((null (cdr ranges))
           (list (cons (car ranges)
                       acc)
                 sumacc))
          (t
           (if (<= b1 (1+ a2)) ;; overlapping
               (remove-overlapping-2 (cons (list (min a1 b1)
                                                 (max a2 b2))
                                           (cddr ranges))
                                     acc
                                     sumacc)
               (remove-overlapping-2 (cdr ranges)
                                     (cons (car ranges)
                                           acc)
                                     (+ 1 (- (max a2 b2)
                                             (min a1 b1))
                                        sumacc)))))))

(defun sum-ranges (ranges)
  (declare (type list ranges))
  (cond ((null ranges)
         0)
        ((eq (length ranges) 1)
         (+ 1 (- (cadar ranges)
                 (caar ranges))))
        (t
         (let ((rr (remove-overlapping ranges)))
           (format t "AAAAAAAAAAAAAAa ~A~%" rr)))))

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
  (let* ((nmk (split-and-parse (read-line stream)))
         (n (nth 0 nmk))
         (m (nth 1 nmk))
         (k (nth 2 nmk))
         (rh (make-hash-table)))
    (declare (type fixnum n m k))
    (loop
       for l from 1 to k
       for rl = (split-and-parse (read-line stream))
       do
         (push (cdr rl)
               (gethash (car rl) rh)))
    (princ
     (- (* m n)
        (loop
           for r from 1 to n
           sum (sum-ranges (gethash r rh)))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input06" :type "txt"))
    (solution s)))

(main)
