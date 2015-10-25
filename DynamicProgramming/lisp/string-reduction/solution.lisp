(defun possible-value (prev current)
  (cond ((eq prev #\a) (cond ((eq current #\b) #\c)
                             ((eq current #\c) #\b)
                             (T nil)))
        ((eq prev #\b) (cond ((eq current #\c) #\a)
                             ((eq current #\a) #\c)
                             (T nil)))
        ((eq prev #\c) (cond ((eq current #\a) #\b)
                             ((eq current #\b) #\a)
                             (T nil)))))

(defun list-different (str)
  (let ((prev (car str)))
    (loop for x in (cdr str)
       for y = 1 then (1+ y)
       when (not (eq x prev))
       collect (list y (list prev x) (possible-value prev x))
       do (setf prev x))))

(defun solve-me (str)
  (format t "~&arguments ~A~%" str)
  (princ (list-different str))
  (terpri))

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

(defun string-to-characters (string)
  (loop for c across string
     collect c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((n (parse-integer (read-line stream))))
    (dotimes (x n)
      (solve-me (string-to-characters (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
