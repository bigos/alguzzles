;;;
(defun product-numstr (s)
  (apply '* (loop for i from 0 below (length s)
               collect (parse-integer (subseq s i (1+ i))))))

(defun substrings (n k nint)
  (loop for x from k to (length nint)
     for i = (- x k)
     maximize (product-numstr (subseq nint i (+ i k)))))

(defun solve-me (n k nint)
  (format t "~&~a~%" (substrings n k nint)))

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
  (let ((tc (parse-integer (read-line stream))))
    (loop for x from 1 to tc do
         (destructuring-bind ((n k) nint)
             (list (split-and-parse (read-line stream))
                   (read-line stream))
           (solve-me n k nint)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
