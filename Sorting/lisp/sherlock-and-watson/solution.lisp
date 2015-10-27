(defun solve-me (n k q a qs)
  (format t "~A ~A ~A ~A ~A" n k q a qs))

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
  (let* ((nkq (split-and-parse (read-line stream)))
         (n (car nkq))
         (k (cadr nkq))
         (q (caddr nkq))
         (a (make-array n :initial-contents (split-and-parse (read-line stream))))
         (qs (loop for x from 1 to q collect (parse-integer (read-line stream)))))
    (solve-me n k q a qs)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
