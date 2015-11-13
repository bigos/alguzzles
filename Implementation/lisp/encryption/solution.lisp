(defun dimentions (l)
  (let* ((s (sqrt l))
         (r (floor s))
         (c (ceiling s)))
    ;; (format t "~A ~A <======~%" (* r c) l)
    (if (>= (* r c) l)
        (list c r)
        (list c (1+ r)))))

(defun strip-chars (str chars)
  (remove-if (lambda (ch) (find ch chars)) str))

(defun i2co (x cs)
  (cons
   (mod x cs)
   (/ (- x (mod x cs)) cs)))

(defun solve-me (enc)
  (let ((ar)
        (dimentions)
        (rs)
        (cs))
    (setf enc (strip-chars enc '(#\Space)))
    ;; (format t "~A ~A~%" enc (length enc))
    (setf dimentions  (dimentions (length enc)))
    ;; (format t "~A dimentions  ~a length ~%" dimentions (length enc))
    (setf ar (make-array dimentions :initial-element #\+)
          rs (car dimentions)
          cs (cadr dimentions))
    ;; (format t "empty array ~A~%" ar)
    (loop for c across enc
       for x from 0 below (length enc)
       for cc = (i2co x rs)
       do (setf (aref ar (car cc) (cdr cc)) c))
    ;; (format t "fille the array ~A~%" ar)
    (loop for r from 0 below rs do
         (loop for c from 0 below cs
            for zn = (aref ar r c) do
              (unless (eql zn #\+)
                (princ zn)))
         (when (< r rs) (princ #\space)))))

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
                      :name "input1" :type "txt"))
    (solution s)))

(main)
