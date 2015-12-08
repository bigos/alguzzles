(defun solve-me (l &optional (r 0) (g 0) (y 0) (b 0))
  ;; finish me
  (if (null l)
      'finished-result
      (solve-me (cdr l)
                (+ r (v (car l)))
                (+ g (v (car l)))
                (+ y (v (car l)))
                (+ b (v (car l)))
                )))

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
  (let ((tc (parse-integer (read-line stream))))
    (loop for c from 1 to tc do
         (format t "~A~%" (if (solve-me (loop for c
                                           across (read-line stream)
                                           collect (format nil "~A" (char-upcase c))))
                              "True"
                              "False")))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
