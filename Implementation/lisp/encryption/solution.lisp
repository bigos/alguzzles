(defun dimentions (str)
  (let* ((l (length str))
         (s (sqrt l))
         (r (floor s))
         (c (ceiling s)))
    (list c r)))

(defun strip-chars (str chars)
  (remove-if (lambda (ch) (find ch chars)) str))

(defun i2co (x cs)
  (cons
   (mod x cs)
   (/ (- x (mod x cs)) cs)
   ))

(defun solve-me (enc)
  (let ((ar)
        (dimentions)
        (rs)
        (cs))
    (setf enc (strip-chars enc '(#\Space)))
    (setf dimentions  (dimentions enc))
    (format t "dimentions  ~A~%" dimentions)
    (setf ar (make-array dimentions)
          rs (car dimentions)
          cs (cadr dimentions))
    (loop for c across enc
       for x from 0 below (length enc)
       for cc = (i2co x rs)
       do
         (setf rx (floor x rs)
               cx (mod x cs))
         (format t "~A ~A~%" (car cc) (cdr cc))
          (setf (aref ar (car cc) (cdr cc)) c)
         )
    (princ ar)))

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
