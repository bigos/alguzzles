(defun solve-me (n)
  (princ "Hello World")
  (terpri)
  (when (> n 1)
    (solve-me (1- n)))
  )

(defun solution (&optional stream)
  (let* ((n (parse-integer (read-line stream))))
    (solve-me n)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
