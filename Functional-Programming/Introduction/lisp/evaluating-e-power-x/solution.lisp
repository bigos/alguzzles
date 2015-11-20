(defun fact (n)
  (if (< n 2)
      1
      (* n (fact(- n 1)))))

(defun pel (x n)
  ;;(format t "~&~a^~A / ~A   ====~%" x n (fact n))
  (/ (expt x n)
     (fact n)))

(defun solve-me (x)
  (format t "~D~%" (* 1.0 (loop for a from 0 below 10 sum (pel x a) ))))

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
