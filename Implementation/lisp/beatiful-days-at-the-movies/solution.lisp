(defun revnum (x)
  (parse-integer
   (reverse
    (format nil "~d" x))))

(defun solve-me (i j k)
  ;; (format t "~A ~A ~A~%" i j k)
  (format t "~A~%"
          (loop for x from i to j
             when (integerp (/ (- x (revnum x))
                               k))
             count x)))

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
  (let ((ijk (split-and-parse (read-line stream))))
    (solve-me (car ijk)
              (cadr ijk)
              (caddr ijk))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
