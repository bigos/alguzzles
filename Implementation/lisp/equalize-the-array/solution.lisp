(defun solve-me (a)
  ;(format t "~A~%" a)
  (let ((ht (make-hash-table))
        (ls))
      (loop for c in a do
           (incf (gethash c ht 0)))

      (setf ls (loop for k being the hash-keys in ht maximize (gethash k ht)))
      (format t "~A~%" (- (length a) ls))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((n (read-line stream))
        (a (split-and-parse (read-line stream))))
    (declare (ignore n))
    (solve-me a)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
