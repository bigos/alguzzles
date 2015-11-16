(defun solve-me (orders)
  (let* ((sorted (sort  orders
                        (lambda (x y) (< (+ (elt x 1) (elt x 2))
                                         (+ (elt y 1) (elt y 2))))))
         (d))
    (setf d (loop for o in sorted collect (car o)))
    (princ (car d))
    (loop for x in (cdr d) do (format t " ~A" x))))

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
  (let* ((tc (parse-integer (read-line stream)))
         (orders
          (loop for x from 1 to tc
             for o = (split-and-parse (read-line stream))
             collect (list x (car o) (cadr o)))))
    (solve-me orders)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input1" :type "txt"))
    (solution s)))

(main)
