(defun pearson-correlation-coefficient (datax datay)
  (let ((meanx (mean datax))
        (meany (mean datay)))
    (/
     (loop
        for xi in datax
        for yi in datay
        summing (* (- xi meanx) (- yi meany)))
     (* (sqrt
         (loop for xi in datax summing (expt (- xi meanx) 2)))
        (sqrt
         (loop for yi in datay summing (expt (- yi meany) 2)))))))

(defun mean (data)
  (/ (loop for d in data summing d) (length data)))

(defun sample (a b)
  (pearson-correlation-coefficient a b))

(defun solve-me (data)
  (let ((x (loop for d in data collect (first  d)))
        (y (loop for d in data collect (second d)))
        (z (loop for d in data collect (third  d))))
    ;; (format t "data ~% ~a~% ~a~% ~a~%" x y z)
    (format t "~&~,2f~%~,2f~%~,2f~%"
            (sample x y)
            (sample y z)
            (sample z x))))

(defun split-by-one-tab (string)
  (loop for i = 0 then (+ j 1)
     as j = (position #\Tab string :start i)  ; remember to change #\Space to #\Tab when submitting
     collect (subseq string i j)
     while j))

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

(defun jp-parse-float (str)
  (labels ((jp-parse-integer (n)
             (if (equal n "")
                 0
                 (parse-integer n))))
    (let ((decimal-point (position #\. str)))
      (if decimal-point
          (let ((integer-part (subseq str 0 decimal-point))
                (decimal-part (subseq str (1+ decimal-point))))
              (* 1.0 (+ (jp-parse-integer integer-part)
                        (/ (jp-parse-integer decimal-part)
                           (expt 10 (length decimal-part))))))
            (parse-integer str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let* ((n (parse-integer (read-line stream)))
         (data (loop for x from 1 to n
                  collecting
                    (mapcar (lambda (x) (parse-integer x))
                            (split-by-one-tab (read-line stream))))))
    (solve-me data)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input01" :type "txt"))
    (solution s)))

(main)
