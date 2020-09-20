(declaim (optimize (speed 0) (debug 3)))

(defun add-1-str (s)
  (format nil "~a" (1+ (parse-integer s))))

;; (solve-inner "12345" 0 1)
(defun solve-inner (str begi en)
  (let ((nx (add-1-str (subseq str begi en))))
    (format t "~A~%" nx)
    (solve-inner str
                 en
                 (+ en 1))))

(defun solve-me (s)
  (format t "~A ------~%" s)

  )

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
  (let (sl )
    (loop for x from 1 to sl do
      (solve-me
       (read-line stream)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
