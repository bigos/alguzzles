(declaim (optimize (debug 3)))

(defun range-values (s e)
  (loop for x from (min s e) to (max s e) collect x))

(defun range-length (s e)
  (1+ (- (max s e) (min s e))))

(defun find-val (columns ranges)
  (if (null ranges)
      columns
      (- columns (apply 'range-length (car ranges)))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let* ((nmk (split-and-parse (read-line stream)))
         (n (nth 0 nmk))
         (m (nth 1 nmk))
         (k (nth 2 nmk))
         (rh (make-hash-table)))
    (loop
       for i from 0 below k
       for read-row = (split-and-parse (read-line stream))
       do
         (push (cdr read-row) (gethash (car read-row) rh)))
    (princ
     (loop for i from 1 to n
        sum
          (find-val
           m
           (gethash i rh ))))
    ))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input06" :type "txt"))
    (solution s)))

(main)
