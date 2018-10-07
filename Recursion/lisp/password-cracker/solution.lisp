(defun solve-me (n keys pass acc)
  (if (equalp pass "")
      (reverse acc)
      (loop for k in keys
            when
            (let ((d (search k pass)))
              (if (null d)
                  nil
                  (zerop d)))
            collect
            (solve-me n
                      keys
                      (subseq pass (length k))
                      (cons k acc)))))

(defun unwind (l)
  (if (consp (car l))
      (unwind (car l))
      l))

(defun list-to-string (my-list &optional (separator " "))
  (let ((result))
    (dolist (item my-list)
      (setf result (concatenate 'string
                                result
                                (format nil "~A~a" separator item))))
    (subseq result 1)))
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
    (loop for c from 1 to tc
          do (progn
               (let ((result
                       (unwind
                        (solve-me
                         (parse-integer (read-line stream))
                         (split-by-one-space (read-line stream))
                         (read-line stream)
                         nil))))
                 (if (equal result '(nil))
                     (format t "WRONG PASSWORD~%")
                     (format t "~a~%" (list-to-string result))))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input21" :type "txt"))
    (solution s)))

(main)
