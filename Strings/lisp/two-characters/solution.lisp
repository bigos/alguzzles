(defun solve-me (sl s)
  (format t "~A ~A~%" sl s)
  (let ((letter-counts (make-hash-table))
        (index-letters (make-hash-table)))
    (loop
       for c across s
       for i = 0 then (1+ i) do
         (incf (gethash c letter-counts 0))
         (setf (gethash i index-letters) c))

    (princ
     (sort
      (loop for k
         being the hash-keys
         in letter-counts
         using (hash-value v)
         collect (list k v))
      (lambda (x y) (>= (cadr x) (cadr y)))))
    (terpri)

    (princ

     (loop for v
        being the hash-values
        in index-letters
        using (hash-key k)

        when (or (eql v #\a) (eql v #\b))

        collect (list k v ))

      )
    (terpri)
    ))

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
  (let ((sl (parse-integer (read-line stream)))
        (s (read-line stream)))
    (solve-me sl s)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
