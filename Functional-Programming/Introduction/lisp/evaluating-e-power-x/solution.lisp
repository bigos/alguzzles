(defun fact (n)
  (if (< n 2)
      1
      (* n (fact(- n 1)))))

(defun integral (n)
  (if (zerop n)
      0
      (+ n (integral (1- n)))))

(defun pel (x n)
  (if (zerop x)
      1
      (/ (expt x n)
         (fact n))))

(defun solve-me (x)
  (format t "the x is ~a~%" x)
  (format t "~A~%" (+ 1.0 (loop for n from 1 below 10 sum (pel x n )))))

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
  (let ((n (parse-integer (read-line stream))))
    (dotimes (_ n)
      (solve-me
       (read-from-string (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
