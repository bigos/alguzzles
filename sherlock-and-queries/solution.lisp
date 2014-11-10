(declaim (optimize (speed 3) (debug 0) (safety 1)))

(defun split-by-one-space (string)
  (declare (type string string))
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun puzzle (m n a b c)
  (declare (optimize (speed 3)))
  (declare (type fixnum m))
  (declare (type fixnum n))
  (declare (type  (simple-array fixnum (*)) a))
  (declare (type  (simple-array fixnum (*)) b))
  (declare (type  (simple-array fixnum (*)) c))
  (let ((big-no (+ (expt 10 9) 7)))

    (loop for i fixnum   from 0 below m do
         (loop for j fixnum from (1- (aref b i)) below n by (aref b i) do
              (progn
                (setf (aref a j) (mod (* (aref a j)
                                         (aref c i))
                                      big-no)))))
    (format t "~&finished~%")
    ;; (loop for x '(unsigned-byte 64) from 0 below n do
    ;;      (princ  (aref a x))
    ;;       (princ " ")
    ;;      )
    ))

(defun solution (&optional stream)
  (let* ((first-line (split-and-parse (read-line stream)))
         (n (car first-line))
         (m (cadr first-line))
         (a (make-array (list n) :element-type 'fixnum :initial-contents (split-and-parse (read-line stream))))
         (b (make-array (list m) :element-type 'fixnum :initial-contents (split-and-parse (read-line stream))))
         (c (make-array (list m) :element-type 'fixnum :initial-contents (split-and-parse (read-line stream)))))
    (format nil "~A ~%" (type-of a))
    (require :sb-sprof)
    (puzzle m n a b c)
    ))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if (search "chess" (machine-instance))
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
