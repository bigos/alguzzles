(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun puzzle (m n a b c)

  (let ((big-no (+ (expt 10 9) 7)))
    (loop for i from 0 below m do
         (loop for j from (1- (aref b i)) below n by (aref b i) do
              (setf (aref a j) (mod (* (aref a j)
                                       (aref c i))
                                    big-no))))
    (loop for x from 0 below n do
         (format t "~A "  (aref a x)))))

(defun solution (&optional stream)
  (let* ((first-line (split-and-parse (read-line stream)))
         (n (car first-line))
         (m (cadr first-line))
         (a (make-array n :element-type 'fixnum :initial-contents (split-and-parse (read-line stream))))
         (b (make-array m :element-type 'fixnum :initial-contents (split-and-parse (read-line stream))))
         (c (make-array m :element-type 'fixnum :initial-contents (split-and-parse (read-line stream)))))
    (puzzle m n a b c)
    ))

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
