;;; solution

(defun as-chars (s)
  (loop for c across s collecting c))

(defun numbers () (as-chars "0123456789"))
(defun lower_case () (as-chars "abcdefghijklmnopqrstuvwxyz"))
(defun upper_case () (as-chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(defun special_characters () (as-chars "!@#$%^&*()-+"))

(defun classify (c)
  (let ((r))
    (loop for f in '(numbers lower_case upper_case special_characters)
          when (member c (funcall f))
            do (setf r f))
    r))

(defun solve-me (n s)
  (declare (ignore n))
  (let* ((classification (loop for c across s collect (classify c)))
         (uniques (remove-duplicates classification)))
    ;(format t "=== ~A  ~A~%" (length classification) (length uniques))
    (format t "~A~%"
            (max (- 4 (length uniques))
                 (- 6 (length s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (let* ((tc (parse-integer (read-line stream))))
    (solve-me  tc
               (read-line stream))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
