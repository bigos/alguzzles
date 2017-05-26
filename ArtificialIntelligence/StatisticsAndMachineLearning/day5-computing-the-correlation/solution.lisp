(defun solve-me (data)
  (format t "data ~a~%" data))

(defun split-by-one-tab (string)
  (loop for i = 0 then (+ j 2)
     as j = (position #\Space string :start i)  ; remember to change #\Space to #\Tab when submitting
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
                      :name "input0" :type "txt"))
    (solution s)))

(main)
