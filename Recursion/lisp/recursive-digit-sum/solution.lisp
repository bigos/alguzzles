(let ((cache (make-hash-table)))
  (defun charnum (c)
    (or (gethash c cache)
        (setf (gethash c cache)
              (- (char-code c) 48)))))

(defun split-add (bs)
  (format nil "~d"
          (loop for i across bs
             sum (charnum i))))

(defun recsolve (bs)
  (cond
    ((eq 1 (length bs)) bs)
    (T (recsolve (split-add bs)))))

(defun solve-me (n k)
  (let* ((bstr (format nil "~d" n))
         (bs (format nil "~a" (loop for c across bstr
                                 sum (* k (charnum c))))))
    (format t "~a"
            (recsolve bs))))

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
  (let ((nx (split-and-parse (read-line stream))))
    (solve-me (car nx) (cadr nx))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input06" :type "txt"))
    (solution s)))

(main)
