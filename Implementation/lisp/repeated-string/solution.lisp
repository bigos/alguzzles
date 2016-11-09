(defun solve-me (s n)
  (let ((ac (loop for c across s
               when (eq c #\a)
               count c))
        (full-count (- n (mod n (length s)))))
    (format T "~a~%" (+ (* ac
                           (/ full-count (length s)))
                        (loop for c across s for x from 1
                           when (and (<= x (rem n (length s)))
                                     (eq c #\a))
                           count c)))))

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
  (let ((s (read-line stream))
        (n (parse-integer (read-line stream))))
    (solve-me s n)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
