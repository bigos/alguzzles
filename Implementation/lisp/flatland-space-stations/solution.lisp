(defun city-map (n mi)
  (let ((cc (make-array n :initial-element nil)))
    (loop for x in mi do
         (setf (aref cc x) T))
    cc))

(defun find-prev (n i mv)
  (loop for x from i downto 0
     until (aref mv x)
     finally (return (- i x))))

(defun find-next (n i mv)
  (loop for x from i below n
     until (aref mv x)
       finally (return (- x i))))


(defun find-nearest (n i mv)
  (if (aref mv i)
      0
      (min
       (find-prev n i mv)
       (find-next n i mv))))

(defun solve-me (n m mi)
  (declare (ignore m))
  (let ((mv (city-map n mi)))
    (format t "~A~%"
            (loop for x from 0 below n maximize (find-nearest n x mv)))))

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
