(declaim (optimize (speed 3) (safety 0) (space 0)))

(defun binary-search (value array)
  (declare (optimize (speed 3)))
  (declare (type fixnum value))
  (declare (type (array fixnum *) array))
  (let ((low 0)
        (high (1- (length array))))
    (declare (type fixnum low high))
    (do () ((< high low) nil)
      (let ((middle (floor (/ (+ low high) 2))))
        (declare (type fixnum middle))
        (cond ((> (aref array middle) value)
               (setf high (1- middle)))
              ((< (aref array middle) value)
               (setf low (1+ middle)))
              (t (return middle)))))))

(defun solve-me (n k set)
  (let ((diffs 0)
        (a (make-array n :initial-element 0))
        (largest 0)
        (res 0)
        (v 0))
    (loop for i from 0 below n do
          (setf (elt a i) (elt set i)))
    (setq largest (elt a (- n 1)))
    (loop for x from 0 below n do
          (setq v (elt a x))
          (setq res (+ v k))
          (when (<= res largest)
            (when (binary-search v a)
              (incf diffs))))
    diffs))

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
  (let ((l1 (split-and-parse (read-line stream)))
        (l2 (sort (split-and-parse (read-line stream)) #'<)))
    (princ (solve-me (car l1) (cadr l1) l2))))

 ;; (solution) ; uncomment this when running on hacker-rank

;;; still need to add  removing vertices
(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "Search/lisp/pairs/"
                                    "input10.txt"))
      (solution s))))
