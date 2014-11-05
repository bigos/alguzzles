(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun melt (seq i)
  (nth (1- i) seq))

(defun puzzle (m n a b c)
  (loop for i from 1 to m do
       (loop for j from 1 to n do
            (progn
              (if (zerop (mod j (melt b i)))
                  (setf (nth (1- j) a ) (* (melt a j)
                                           (melt c i)))))))
  (loop for x below n do
       (format t
               "~A "
               (mod (nth x a) (+  (expt 10 9) 7)))))

(defun solution (&optional stream)
  (let* ((first-line (split-and-parse (read-line stream)))
         (n (car first-line))
         (m (cadr first-line))
         (a (split-and-parse (read-line stream)))
         (b (split-and-parse (read-line stream)))
         (c (split-and-parse (read-line stream))))
    (puzzle m n a b c)))

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
      (solution s))
    (format t "~&=========================~%")
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input00.txt"))
      (solution s))
    (format t "~&=========================~%")
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input13.txt"))
      (solution s))
    ))

(repl-main)
