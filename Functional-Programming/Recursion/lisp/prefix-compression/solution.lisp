(defun rec (x y res)
  (if (and (< res (length x))
           (< res (length y))
           (equalp (elt x res) (elt y res)))
      (rec x y (1+ res))
      res))

(defun solve-me (x y)
  (let ((prefix-len (rec x y 0)))
    (format t "~A ~A~%~A ~A~%~A ~A"
            prefix-len (subseq x 0 prefix-len)
            (- (length x) prefix-len) (subseq x prefix-len)
            (- (length y) prefix-len) (subseq y prefix-len)
            )))

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
  (let ((x (read-line stream))
        (y (read-line stream)))
    (solve-me x y)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
