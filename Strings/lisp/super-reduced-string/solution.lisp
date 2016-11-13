(defun two-same-p (s n)
  (equal (subseq s (+ n 0) (+ n 1))
         (subseq s (+ n 1) (+ n 2))))

(defun print-result (s)
  (format t "~A~%"
          (if (zerop (length s))
              "Empty String"
              s)))

(defun solve-me (s n)
  (if (>= (+ n 1) (length s))
      (print-result s)
      (if (two-same-p s n)
          (solve-me
           (concatenate 'string
                        (subseq s 0 n)
                        ""
                        (subseq s (+ n 2)))
           0)
          (solve-me
           s
           (1+ n)))))

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
  (let ((s (read-line stream)))
    (solve-me s 0)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
