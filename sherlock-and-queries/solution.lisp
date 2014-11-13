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

(defun naiive (a b)
  (let ((big-no 1000000007))
    (declare (type integer a b))
    (declare (type integer  big-no))
    ;(format t ":::: ~A ~A~%" a b)
    (mod (* a b)
            big-no)))

;; (defun montgomery (x y)
;;   (let ((big-no (+ (expt 10 9) 7))
;;         (a 0))
;;     (declare (type fixnum  big-no a x y))
;;     (loop for k from 0 below m do
;;          )))

;; http://stackoverflow.com/questions/9009139/optimising-multiplication-modulo-a-small-prime
;; read about montgomery reduction

(defun puzzle (m n a b c)
  (declare (optimize (speed 3)))
  (declare (type integer m))
  (declare (type integer n))
  (declare (type (simple-array integer (*)) a))
  (declare (type (simple-array integer (*)) b))
  (declare (type (simple-array integer (*)) c))
  ;(setf n 2 m 2)
  (loop for i of-type integer  from 0 below m do
       (loop for j of-type integer from (1- (aref b i)) below n by (aref b i) do
            (setf (aref a j) (naiive (aref a j) (aref c i)))))
  (format t "~&finished~%")
  (loop for x from 0 below n do
       (princ  (aref a x))
       (princ " ")))

(defun solution (&optional stream)
  (let* ((first-line (split-and-parse (read-line stream)))
         (n (car first-line))
         (m (cadr first-line))
         (a (make-array (list n) :element-type 'unsigned-byte
                        :initial-contents (split-and-parse (read-line stream))))
         (b (make-array (list m) :element-type 'unsigned-byte
                        :initial-contents (split-and-parse (read-line stream))))
         (c (make-array (list m) :element-type 'unsigned-byte
                        :initial-contents (split-and-parse (read-line stream)))))
    (format nil "~A ~%" (type-of a))
    (require :sb-sprof)
    (puzzle m n a b c)
    ))

  ;; (solution) ; uncomment this when running on hacker-rank


(defun repl-main ()
  (let ((path (if (search "chess" (the string (machine-instance)))
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
