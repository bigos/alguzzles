(defun solve-me (n m k ii)
  (format t "====== ~s ~s ~s ~s ~%" n m k ii)
  (let ((row-hash (make-hash-table)))
    (loop for l in ii do
         (setf (gethash (car l) row-hash) (union (cdr l) (ge)))
         )))

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
  (let* ((nmk (split-and-parse (read-line stream))))
    (solve-me (nth 0 nmk)
              (nth 1 nmk)
              (nth 2 nmk)
              (loop for x from 0 below (nth 2 nmk)
                 collect (split-and-parse (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
