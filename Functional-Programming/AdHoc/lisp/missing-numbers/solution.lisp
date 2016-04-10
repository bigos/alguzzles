;; (defun hash-keys (hash)
;;   (let ((keys))
;;     (maphash (lambda (k v) (push (cons k v) keys)) hash)
;;     (reverse keys)))

(defun solve-me (a b)
  (let ((ht (make-hash-table))
        (diffs))
    (loop for ax in a
         for v = (gethash ax ht (cons 0 0))
       do
         (incf (car v))
         (setf (gethash ax ht) v))
    (loop for bx in b
       for v = (gethash bx ht (cons 0 0))
       do
         (incf (cdr v))
         (setf (gethash bx ht) v))
    (maphash (lambda (k v) (if (not (eq (car v) (cdr v))) (push k diffs))) ht)
    (loop for v in (sort diffs '<)
       for sep = "" then " " do
         (format t "~A~A" sep v))
    diffs))

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
  (let ((length-a (parse-integer (read-line stream)))
        (data-a (split-and-parse (read-line stream)))
        (length-b (parse-integer (read-line stream)))
        (data-b (split-and-parse (read-line stream))))
    (solve-me data-a data-b)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
