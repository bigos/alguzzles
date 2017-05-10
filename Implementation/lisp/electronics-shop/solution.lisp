(defun solve-me (s n m kbs pds)
  ;; (format t "=== ~a ~A ~A === ~A ~A"  s n m kbs pds)
  (declare (ignore n m))
  (let ((result (loop for k in kbs
                   maximize (loop for p in pds
                               for tot = (+ k p)
                               when (<= tot s)
                               maximize tot))))
    (format t "~a" (if (zerop result)
                       -1
                       result))))

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
  (let ((snm (split-and-parse (read-line stream)))
        (kbs (split-and-parse (read-line stream)))
        (pds (split-and-parse (read-line stream))))
    (destructuring-bind (s n m)
        snm
      (solve-me s n m kbs pds))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
