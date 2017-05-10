(defun page-count (p)
  (if (oddp p)
      (1- p)
      p))

(defun f2b (n p)
  (/ (page-count p)
     2))

(defun b2f (n p)
  (/ (- (if (oddp n)
            (1- n)
            n)
        (page-count p))
     2))

(defun solve-me (n p)
  (format t "~a" (min
                  (f2b 0 p)
                  (b2f n p))))

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
  (let ((n (parse-integer (read-line stream)))
        (p (parse-integer (read-line stream))))
    (solve-me n p)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
