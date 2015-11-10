(defun solve-me (rcg  rcp ar ap)
  (format t "~&~a~%~a~%~A~%~A~%" rcg  rcp ar ap))

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
        (r) (p)
        (ar) (ap))
    (dotimes (x tc)
      (setf rcg (split-and-parse (read-line stream)))
      (setf r (loop for r below (car rcg)
                 collecting (loop for c across (read-line stream) collect c)))
      (setf ar (make-array rcg :initial-contents r))
      (setf rcp (split-and-parse (read-line stream)))
      (setf p (loop for r below (car rcp)
                 collecting (loop for c across (read-line stream) collect c)))
      (setf ap (make-array rcp :initial-contents p))
      (solve-me rcg rcp  ar ap))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
