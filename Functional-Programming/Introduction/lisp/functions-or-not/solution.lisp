(defun first-els (l)
  (map 'list #'car l))

(defun second-els (l)
  (map 'list #'cadr l))

(defun solve-me (n)
  (if (eq (length (remove-duplicates (first-els n)))
          (length (second-els n)))
      "YES"
      "NO"))

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
  (let ((tc (parse-integer (read-line stream))))
    (loop for x from 1 to tc
       for pairs-in-tc = (parse-integer (read-line stream)) do
         (format t "~A~%" (solve-me
                           (loop for y from 1 to pairs-in-tc
                              collect (split-and-parse (read-line stream))))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
