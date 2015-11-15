(defun indexed-subarray (a start end)
  (declare (optimize (speed 3) (safety 0)))
  (if (zerop start)
      (aref a end)
      (- (aref a end) (aref a (1- start)))))

(defun max-subarray (a n m)
  (declare (optimize (speed 3) (safety 0))
           (inline indexed-subarray))
  (let ((indexes (make-array n))
        (my-max 0)
        (found nil))
    (loop
       for x across a
       for i = 0 then (1+ i)
       for y = (aref a 0) then (+ x y) do
         (setf (aref indexes i) y))
    (loop for s from 0 below n do
         (loop for x from s below n do
              (setf my-max (max my-max
                                (mod (indexed-subarray indexes s x)
                                     m)))))
    my-max))

(defun solve-me (nm ints)
  (let ((a (make-array (car nm) :initial-contents ints)))
    (princ (max-subarray a (car nm) (cadr nm)))
    (terpri)))

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
    (dotimes (x tc)
      (solve-me (split-and-parse (read-line stream))
                (split-and-parse (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input01" :type "txt"))
    (solution s)))

(main)
