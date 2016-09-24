;;; (declaim (optimize (debug 3)))

(defun rec-solve (steps s)
  (if (< (length s) 3)
      steps
      (if (equal (take 3 s) '(0 1 0))
          (rec-solve (1+ steps)
                     (cons (cadr s)
                           (cons 1
                                 (cdr    (cdr (cdr s))))))
          (rec-solve steps
                     (cdr s)))))

(defun solve-me (n b)
  (declare (ignore n))
  (format t "~A~%" (rec-solve 0 b)))

(defun take (n seq)
  (subseq seq 0 n))

(defun drop (n seq)
  (subseq seq n))

(defun str2num (s)
  (loop for i  from 0 below (length s) collect (parse-integer (subseq s i (1+ i)))))

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
        (b (str2num (read-line stream))))
    (solve-me n b)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
