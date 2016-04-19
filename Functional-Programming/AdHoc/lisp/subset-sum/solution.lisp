(declaim (optimize (space 0) (safety 0) (speed 3)))

;;; we do not need to use combinations, if we sort the numbers we can figure out minimal set for required number, duh
(defun solve-me (a s)
  (loop for x from 1 upto (length a)
     for y = (every 'null (map 'list (lambda (x) (>= (apply '+ x) s) ) (comb x a)))
     while y
     finally (return (if (<= x (length a))
                         x
                         -1))))

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
