(proclaim '(optimize (speed 3) (safety 0)))

(defparameter *cnt* 0)

(defun pattern (n)
  (loop for x from 0 to n do
       (format t "~6,B ~3,d ~a ~3,d ~3,d~%" x x
               (if (eq (+ n x) (logxor n x))
                   #\= #\!)
               (+ n x) (logxor n x))))

(defun solve-me (n)
  (loop for x from 0 to n do
       (when (eq (+ n x)
                 (logxor n x))
         (incf *cnt*)))
  (princ *cnt*)
  (terpri))

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
  (let ((n (parse-integer (read-line stream))))
    (solve-me n)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
