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
  (let ((n (parse-integer (read-line stream)))
        (ints))
    (dotimes (x n)
      (push (split-and-parse (read-line stream)) ints))
    (princ ints)))

;; (solution) ; uncomment this when running on hacker-rank


(with-open-file (s (make-pathname
                    :directory
                    (pathname-directory
                     (parse-namestring *load-pathname*))
                    :name "input0" :type "txt"))
  (solution s))
