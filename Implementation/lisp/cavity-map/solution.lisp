(defun solve-me (n ints)
  (format t "~A" ints)
  (let ((m (make-array (list n n))))
    (loop for r from 0 below n do
         (loop for c from 0 below n ;do
              ;(setf (aref r c) (elt  (elt ints r) c))
              ))
    (princ m)))

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

(defun parsed-digits (string)
  (loop for c across string
     collect (parse-integer (format nil "~c" c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((n (parse-integer (read-line stream)))
        (ints))
    (dotimes (x n)
      (setf ints (concatenate 'list ints (parsed-digits (read-line stream)))))
    (solve-me n ints)))

;; (solution) ; uncomment this when running on hacker-rank


(with-open-file (s (make-pathname
                    :directory
                    (pathname-directory
                     (parse-namestring *load-pathname*))
                    :name "input0" :type "txt"))
  (solution s))
