(defun degeneratep (a b c) (eql a (+ b c)))

(defun solve-me (n ll)
  (let ((a (car ll))
        (b (cadr ll))
        (c (caddr ll)))
    ;(format t " ==== ~A ~A~%" n ll )
    (cond
      ((and (degeneratep a b c)
            (eql n 3))
       (list -1))
      ((degeneratep a b c)
       (solve-me (1- n) (cdr ll)))
      ((not (degeneratep a b c))
       (list c b a)))))

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
  (let* ((n (parse-integer (read-line stream)))
         (ll (split-and-parse (read-line stream))))
    (loop for c in (solve-me n (sort ll '>))
       for nl = "" then " "
         do
         (format t "~a~A" nl c))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input1" :type "txt"))
    (solution s)))

(main)
