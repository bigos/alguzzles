(proclaim '(optimize (debug 3)))

(defun solve-me (l &optional (r 0) (g 0) (y 0) (b 0))
  ;; finish me

  (cond ((equalp (car l) "R") (setf r (1+ r)))
        ((equalp (car l) "G") (setf g (1+ g)))
        ((equalp (car l) "Y") (setf y (1+ y)))
        ((equalp (car l) "B") (setf b (1+ b))))

  (format t "~s ~A~A~A~A !!!! ~%" l r g y b)

  (if (null l)
      'finished-result
      (solve-me (cdr l)
                r
                g
                y
                b)))

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
