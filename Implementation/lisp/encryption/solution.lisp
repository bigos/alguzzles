(defun dimentions (str)
  (let* ((l (length str))
         (s (sqrt l))
         (r (floor s))
         (c (ceiling s)))
    (list r c)))

(defun strip-chars (str chars)
  (remove-if (lambda (ch) (find ch chars)) str))

(defun solve-me (enc)
  (let ((ar)
        (dimentions)
        (rs)
        (cs))
    (setf enc (strip-chars enc '(#\Space)))
    (setf dimentions (dimentions enc))
    (setf ar (make-array dimentions)
          rs (car dimentions)
          cs (cadr dimentions))
    (loop for c across enc
       for x from 0 below (length enc)
       do
         (setf (aref ar
                     (mod x rs) (floor x cs)) ;wrong
               c)
         )
    (princ ar)
    ))

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
  (let ((enc (read-line stream)))
    (solve-me enc)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
