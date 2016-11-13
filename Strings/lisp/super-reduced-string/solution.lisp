(defun solve-me (s n r)
  (if (>= (+ n 1) (length s))
      r
      (if (equalp (subseq s n (1+ n))
                  (subseq s (+ n 1) (+ n 2)))
          (solve-me s (+ n 2) (push (list n (subseq s n (1+ n)) ) r))
          (solve-me s (+ n 1) (push (subseq s n (1+ n)) r)))))

(defun solve-me-once (s n r)
  (let ((singles (solve-me s n r))
        (r))

    (loop for i in singles do
         (when (atom i)
           (push i r)))

    (if (zerop (length r))
        (format t "Empty String~%")
        (loop for c in r do (princ c))
         )))

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
    (solve-me-once (format nil "~A-" s) 0 nil)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
