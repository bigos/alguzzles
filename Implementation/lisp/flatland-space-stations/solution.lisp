(defun city-map (n mi)
  (let ((cc (make-array n :initial-element nil)))
    (loop for x in mi do
         (setf (aref cc x) T))
    cc))

(defun solve-me (n m mi)
  (declare (ignore m))
  (format t "~A~%" (max
                    ;; beginning to 1st
                    (car mi)
                    ;; end to last
                    (- (1- n) (car (last mi)))
                    ;; middle
                    (floor (/ (- (car (last mi)) (car mi))
                              2)))))

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
  (let ((nm (split-and-parse (read-line stream)))
        (mi (split-and-parse (read-line stream))))
    (solve-me (car nm) (cadr nm) (sort mi #'<))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
