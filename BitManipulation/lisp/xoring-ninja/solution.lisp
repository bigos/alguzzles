(defun comb (m list)
  (let ((result))
    (labels ((comb1 (l c m)
               (when (>= (length l) m)
                 ;; added reversing in the push function
                 (if (zerop m) (return-from comb1 (push (reverse c) result)))
                 (comb1 (cdr l) c m)
                 (comb1 (cdr l) (cons (first l) c) (1- m)))))
      (comb1 list nil m))
    result))

;;; xoring values
;; (logxor 1 2 3)

(defun solve-me (n l)
  ;; (format t "~A ~A~%" n l)
  (let ((combinations
         (loop for x from 1 to n
            collect (comb x l)))
        (res 0))
    (loop for cx in combinations do
         (loop for cy in cx do
              (incf res (apply 'logxor cy))
            ;; (format t "~a ~A ~a~%" cy (apply 'logxor cy) res)
              ))
    (princ (mod res  (+ 7 (expt 10 9))))
    (terpri)))

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
    (dotimes (x tc)
      (solve-me
       (parse-integer (read-line stream))
       (split-and-parse (read-line stream))))))


;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
