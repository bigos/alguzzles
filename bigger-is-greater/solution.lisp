(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun next-perm (vec cmp)  ; modify vector
  (declare (type (simple-array * (*)) vec))
  (macrolet ((el (i) `(aref vec ,i))
             (cmp (i j) `(funcall cmp (el ,i) (el ,j))))
    (loop with len = (1- (length vec))
       for i from (1- len) downto 0
       when (cmp i (1+ i)) do
         (loop for k from len downto i
            when (cmp i k) do
              (rotatef (el i) (el k))
              (setf k (1+ len))
              (loop while (< (incf i) (decf k)) do
                   (rotatef (el i) (el k)))
              (return-from next-perm vec)))))

(defun puzzle (x)
  (let ((o (concatenate 'string "" x)))
    (loop for a = x then (next-perm a #'char<) while a do
         (format t "~a ~a ~a~%" a o (string<  (concatenate 'string "" o) a)))))

(defun solution (&optional stream)
  (let* ((n (parse-integer (read-line stream)))
         (data (loop repeat n
                  collect (read-line stream))))
    (format nil "~A ~A~%" n data)
    (loop for x in data do (puzzle x))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/"))
        (puzzle "bigger-is-greater"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input.1.txt"))
      (solution s))))

(repl-main)
