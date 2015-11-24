(defun solve-me (k)
  (loop for r from 0 below k do
       (format t "~&~a" 1)
       (print-row r 1 r 1)
       (terpri)))


(defun print-row (n top b c)
  (if (> top  n)
      nil
      (progn
        (setf c (* c (/ b top )))
        (format t " ~a" c)
        (print-row n (1+ top) (1- b) c))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((k (parse-integer (read-line stream))))
    (solve-me k)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
