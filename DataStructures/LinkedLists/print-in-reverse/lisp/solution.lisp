;; https://www.hackerrank.com/challenges/print-the-elements-of-a-linked-list-in-reverse/problem

(defun solve-me (d)
  (when (cdr d)
    (solve-me (cdr d)))
  (format t "~A~%" (car d)))

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

(defun string-to-characters (string)
  (loop for c across string
        collect c))

(defun read-all-data (stream)
  (loop for line = (read-line stream nil 'eof)
        until (equal line 'eof)
        collect line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((n (parse-integer (read-line stream))))
    (dotimes (x n)
      (let ((nd (parse-integer (read-line stream))))
        (solve-me
         (loop for x from 1 to nd
               collect (read-line stream)))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
