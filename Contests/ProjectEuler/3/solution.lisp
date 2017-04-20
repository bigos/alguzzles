(let ((cache (make-hash-table)))
  (defun factor (n)
    "Return a list of factors of N."
    (or (gethash n cache)
        (setf (gethash n cache)
              (when (> n 1)
                (loop with max-d = (isqrt n)
                   for d = 2 then (if (evenp d) (+ d 1) (+ d 2)) do
                     (cond ((> d max-d) (return (list n))) ; n is prime
                           ((zerop (rem n d)) (return (cons d (factor (truncate n d))))))))))))

(defun solve-me (n)
  (format t "~A~%" (car (reverse (factor n)))))

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
  (let ((tc (parse-integer (read-line stream))))
    (loop for x from 1 to tc do (solve-me (parse-integer (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
