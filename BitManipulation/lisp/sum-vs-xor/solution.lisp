(proclaim '(optimize (speed 3) (safety 0)))

(defparameter *cnt* 0)

(defun pattern (n)
  (let ((sum-nx)
        (xor-nx))
    (format t "~A binary ~6,B~%~%" n n)
    (loop for x from 0 to n do
         (setf
          sum-nx (+ n x)
          xor-nx (logxor n x))
         (when (eq sum-nx xor-nx)

           (format t "~6,B ~3,d ~a ~3,d ~3,d ~a ~,B~%" x x
                   (if (eq sum-nx xor-nx)
                       #\= #\!)
                   (+ n x)
                   xor-nx
                   (if (zerop (mod x 4)) #\H #\.)
                   xor-nx
                   )))))

(defun solve-me (n)
  (loop for x from 0 to 500 do
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
