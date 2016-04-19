(defun comb (m list s)
  (let ((result))
    (labels ((summing (l)
               (loop for x in l
                  when (listp x) sum (car x)
                  unless (listp x) sum x))
             (comb1 (l c m)
               (when (>= (length l) m)
                 (when (zerop m)

                   ;; higher order function should go here
                   (if (>=  (summing result) s)
                       (return-from comb (length result)))
                   (return-from comb1 (push c result))

                   )
                 (comb1 (cdr l) c m)
                 (comb1 (cdr l) (cons (first l) c) (1- m)))))
      (comb1 list nil m))
    -1))

(defun solve-me (a s)
  (loop for x from 1 to (length a)
     with y = (comb x a s)
     until (> y -1)
       finally (return y)))

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
  (let ((size-of-a (parse-integer (read-line stream)))
        (a (split-and-parse (read-line stream)))
        (tc (parse-integer (read-line stream)))
        (s))
    (declare (ignore size-of-a))
    (loop for x from 1 to tc do
         (setf s (parse-integer (read-line stream)))
         (format t "~a~%" (solve-me a s)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
