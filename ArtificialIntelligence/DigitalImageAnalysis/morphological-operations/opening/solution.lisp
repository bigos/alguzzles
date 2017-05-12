(defun solve-me (w h data mark)
  (let ((d (make-array (list h w)))
        (m (make-array (list 3 3) :initial-contents 1))
        )
    (loop for r from 0 below h
       do (loop for c from 0 below w
             do (setf (aref d r c) (subseq (elt data r) c (1+ c)))))
    (format t "~a~%~a~%" data d)
    ))

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

(defun read-all-data (stream)
  (loop for line = (read-line stream nil 'eof)
     until (equal line 'eof)
     collect line))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((data (read-all-data stream)))
    (format t "~A~%~%" data)
    (let ((h (length data))
          (w (length (first data)))
          (mark '("111" "111" "111")))
      (solve-me w h data mark))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
