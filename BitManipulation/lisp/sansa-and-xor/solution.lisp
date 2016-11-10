(proclaim '(optimize (speed 3) (safety 0)))

(defun looper (n m l &optional (acc 0))
  (if (< m n)
      acc
      (looper n (1- m) (cdr l) (apply #'logxor (cons acc (subseq l 0 n))))))

(defun solve-me (n a)
  ;; (format t "~A ~A ~%" n a)
  (format t "~A~%"
          (apply #'logxor (loop for x from 1 to n collect (looper x n a)))))

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
  (let ((tc (parse-integer (read-line stream)))
        (n))
    (dotimes (x tc)
      (setf n (parse-integer (read-line stream)))
      (solve-me
       n (split-and-parse (read-line stream))))))


;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
