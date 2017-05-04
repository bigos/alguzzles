(defparameter maxcounts '(999 0))

(defun maxb (k v)
  (cond ((> v (cadr maxcounts))
         (setf maxcounts (list k v)))
        ((and (eq v (cadr maxcounts))
              (< k (car maxcounts)))
         (setf maxcounts (list k v)))))

(defun solve-me (n bb)
  (declare (ignore n))
  ;; (format t "~a ~a" n bb)
  (let ((bt (make-hash-table)))
    (loop for bx in bb do
         (incf (gethash bx bt 0)))
    ;; (format t "~a" bt)
    (maphash #'maxb bt)
    (format t "~a~%" (car maxcounts))))


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
  (let ((n (parse-integer (read-line stream)))
        (bb (split-and-parse (read-line stream))))
    (solve-me n bb)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
