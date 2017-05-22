(defun mean (data)
  (/ (apply #'+ data) (length data)))

(defun median (data)
  (let* ((data-length (length data))
         (data-length-halved (/ data-length 2))
         (sorted-data (sort data #'<)))
    (if (oddp (length sorted-data))
        (elt sorted-data (floor data-length-halved))
        (mean (subseq sorted-data
                      (1- data-length-halved)
                      (1+ data-length-halved)
                      )))))

(defun solve-me (data)
  (loop for r in (list (mean data)
                       (median data))
       do (format t "~a~%" (* 1.0 r))))


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
  (let* ((tc (parse-integer (read-line stream)))
         (data (loop for d in (split-by-one-space (read-line stream))
                  collect (jp-parse-float d))))
    (declare (ignore tc))
    (solve-me data)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
