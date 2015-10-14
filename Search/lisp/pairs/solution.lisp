(defun binary-search (value array)
  (let ((low 0)
        (high (1- (length array))))
    (do () ((< high low) nil)
      (let ((middle (floor (/ (+ low high) 2))))
        (cond ((> (aref array middle) value)
               (setf high (1- middle)))
              ((< (aref array middle) value)
               (setf low (1+ middle)))
              (t (return middle)))))))

(defun solve-me (n k set)
  (let ((diffs 0)
        (a (make-array n :initial-element 0)))
    (loop for i from 0 below n do
         (setf (elt a i) (elt set i)))
    (loop for x in set do
         (when (or
                ;; binary search for x + k
                ;; binary search for x - k
                (= 1 1))
           (incf diffs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
       as j = (position #\Space string :start i)
       collect (subseq string i j)
       while j)))

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
                                    "input0.txt"))
      (solution s))))
