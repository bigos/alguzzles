(defun find-it (n)
  (let ((x (expt n 2))
        (str)
        (ls)
        (partl)
        (str1)
        (str2))
    (setf str (with-output-to-string (stream)
                (princ x stream))
          ls (length str)
          partl (floor (/ ls 2))
          str1 (subseq str 0 partl)
          str2 (subseq str partl))
    ;; (format t "~A ~A~%" str1 str2)
    (if (< n 4)
        (if (eq n 1) T nil)
        (eq n (+ (parse-integer str1)
                 (parse-integer str2))))))

(defun solve-me (a b)
  (let ((found))
    (loop for x from a to b do
         (when (find-it x)
           (when found
             (princ  " "))
           (princ x)
           (setf found T)))
    (unless found
      (princ "INVALID RANGE"))))

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
  (let ((a (parse-integer (read-line stream)))
        (b (parse-integer (read-line stream))))
    (solve-me a b)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
