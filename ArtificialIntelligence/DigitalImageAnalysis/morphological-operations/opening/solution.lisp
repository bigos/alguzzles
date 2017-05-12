(declaim (optimize (debug 3) (speed 0)))

(defun los2arr (los &optional (fn 'parse-integer))
  "Load list of strings LOS to array applying FN to every 1 character long substring."
  (let ((w (length (first los)))
        (h (length los)))
    (let ((d (make-array (list h w))))
      (loop for r from 0 below h
         do (loop for c from 0 below w
               do (setf
                   (aref d r c)
                   (funcall fn (subseq (elt los r) c (1+ c))))))
        d)))

(defun solve-me (data mark)
  (let* ((d (los2arr data))
         (d-dim (array-dimensions d))
         (m (make-array (list 3 3) :initial-element 1))
         (m-dim (array-dimensions m))
         (m-origin (cons 1 1)))

    (loop for r
       from (+ 0 1)
       below (- (second d-dim) 1)
       do (loop for c
             from (+ 0 1)
             below (- (first d-dim ) 1)
             do (format t "~&~A - ~A~%" r c))) ; marker coordinates

    (format t "~a~%~a~%~A~%~%" data d m)
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
    (let ((mark '("111" "111" "111")))
      (solve-me data mark))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
