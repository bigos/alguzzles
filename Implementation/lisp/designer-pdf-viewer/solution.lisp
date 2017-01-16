(declaim (optimize (debug 3) (speed 0)))

(defun solve-me (heights str)
  (let ((chars (map 'list (lambda (x) x) str))
        (lh (make-hash-table))
        (charh)
        (tallest))
    (loop
       for h in heights
       for cc from (char-code #\a)
       to (char-code #\z)
       do
         (setf (gethash cc lh) h))
    (setf charh (map 'list
                     (lambda (c) (gethash (char-code c) lh))
                     chars))
    (setf tallest (apply #'max charh))
    ;; (format t "~&==== ~s ~s~%"    charh tallest)
    (princ (* tallest 1 (length chars)))))

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
  (let ((heights (split-and-parse (read-line stream)))
        (str (read-line stream)))
    (solve-me heights str)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input01" :type "txt"))
    (solution s)))

(main)
