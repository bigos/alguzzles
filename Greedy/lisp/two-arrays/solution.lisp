(defun find-solution ( k a b)
  ;; (format t " ~A ~A ~A~%"  k a b)
  (let ((res
         (loop for ax in a
            for bx in b
            collect (+ ax bx))))
    (princ (if (every (lambda (x) (>= x k)) res)
                         "YES"
                         "NO"))
    (terpri)))

(defun solve-me (n k a b)
  (declare (ignore n))
  ;; (format t "~A ~A ~A ~A~%" n k a b)
  (find-solution k (sort a '>) (sort b '<)))

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
        (nk) (nints-a) (nints-b))
    (dotimes (x n)
      (setf nk (split-and-parse (read-line stream)))
      (setf nints-a (split-and-parse (read-line stream)))
      (setf nints-b (split-and-parse (read-line stream)))
      (solve-me (car nk) (cadr nk) nints-a nints-b))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
