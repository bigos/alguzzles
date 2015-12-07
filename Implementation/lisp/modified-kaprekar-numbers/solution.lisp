(defun find-it (x)
  (case x
    ((1 9 45 55 99) T)
    (otherwise nil)))

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
