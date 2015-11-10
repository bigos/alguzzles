(defun solve-me (rcg r rcp p)
  (format t "~&~a~%~a~%~A~%~A" rcg r rcp p))

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
  (let ((tc (parse-integer (read-line stream)))
        (rcg) (rcp)
        (r) (p))
    (dotimes (x tc)
      (setf rcg (split-and-parse (read-line stream)))
      (setf r (loop for r below (car rcg)
                 collecting (split-and-parse (read-line stream))))
      (setf rcp (split-and-parse (read-line stream)))
      (setf p (loop for r below (car rcp)
                 collecting (split-and-parse (read-line stream))))
      (solve-me rcg r rcp p))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
