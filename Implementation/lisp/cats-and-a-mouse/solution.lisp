(defun solve-me (ccm)
  (let ((diff (- (abs (- (car ccm) (caddr ccm)))
                 (abs (- (cadr ccm) (caddr ccm))))))
    (format t "~a~%"
            (cond ((zerop diff) "Mouse C")
                  ((> diff 0) "Cat B")
                  ((< diff 0) "Cat A")))))

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
  (let ((q (parse-integer (read-line stream))))
    (loop
       for x from 0 below q
       for ccm = (split-and-parse (read-line stream))
       do
         (solve-me ccm))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
