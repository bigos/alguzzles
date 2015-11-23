(defun fact (n)
  (if (< n 2)
      1
      (* n (fact(- n 1)))))

(defun rec (k r)
  (loop for n from 0 to r do (format t
                                     (if (zerop n) "~&~a" " ~a")
                                     (/ (fact n)
                                        (fact (* (fact r)
                                                 (- n r))))))
  (if (>= r k)
      T
      (rec k (1+ r))))

(defun solve-me (k)
  (rec k 0)  )

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
  (let ((k (parse-integer (read-line stream))))
    (solve-me k)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
