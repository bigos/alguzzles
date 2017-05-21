(defun solve-me (data)
  (loop for d in data
     for charge-time = (car d)
     for uncapped-discharge-time = (* charge-time 2)
     for discharge-time = (if (>= uncapped-discharge-time 8.0)
                              8.0
                              uncapped-discharge-time)
     do
       (format t "~a~%"  discharge-time)))

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
  (let ((data (loop for tc from 1 to 1
                 for line = (read-line stream)
                 collecting (list (jp-parse-float line)))))
    (solve-me (sort data (lambda (x y) (< (car x) (car y)))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
