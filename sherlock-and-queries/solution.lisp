(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun solution (&optional stream)
  (let* ((first-line (split-and-parse (read-line stream)))
         (n (car first-line))
         (m (cadr first-line))
         (a (split-and-parse (read-line stream)))
         (b (split-and-parse (read-line stream)))
         (c (split-and-parse (read-line stream))))
    (format nil "data: ~A ~A ~A ~A ~A~%" n m a b c)
    (loop for i from 1 to m do
         (loop for j from 1 to n do
              (if (zerop (mod j (elt b i)))
                  (setf (elt a j) (* (elt a j)
                                     (elt c j))))
              (format t "~A " (elt a j))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/"))
        (puzzle "sherlock-and-queries"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input.1.txt"))
      (solution s))))

(repl-main)
