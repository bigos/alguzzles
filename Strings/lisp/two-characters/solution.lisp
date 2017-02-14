;; (comb 2 '(b e a f))
(defun comb (m list)
  (let ((result))
    (labels ((comb1 (l c m)
               (when (>= (length l) m)
                 (if (zerop m) (return-from comb1 (push c result)))
                 (comb1 (cdr l) c m)
                 (comb1 (cdr l) (cons (first l) c) (1- m)))))
      (comb1 list nil m))
    result))

(defun dump-hash (h)
  (loop for k being the hash-keys
     in h
     using (hash-value v) do
       (format t "~&~a => ~a~%" k v)))

(defun solve-me (sl s)
  (format t "~A ~A~%" sl s)
  (let ((letter-counts (make-hash-table))
        (count-letters (make-hash-table))
        (letter-indexes (make-hash-table)))
    (loop
       for c across s
       for i = 0 then (1+ i) do
         (incf (gethash c letter-counts 0))
         (push i (gethash c letter-indexes)))

    (loop
       for p in
         (sort
          (loop for k
             being the hash-keys
             in letter-counts
             using (hash-value v)
             collect (list k v))
          (lambda (x y) (>= (cadr x) (cadr y))))
       do
         (push (car p) (gethash (cadr p) count-letters)))

    (dump-hash count-letters)
    (terpri)

    (dump-hash letter-indexes)
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
