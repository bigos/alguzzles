(defparameter *letter-weights* (make-hash-table))

(loop
   for c from (char-code #\a) to (char-code #\z)
   for w = 1 then (1+ w)
   do (setf (gethash (code-char c) *letter-weights*) w))

(defun weight-of-characters (s)
  (loop for c in s
     sum (gethash c *letter-weights*)))

(defun weight-of-a-string (s)
  (loop for c across s
     sum (gethash c *letter-weights*)))

(defun uniform-strings (s)
  (let ((uniform)
        (current)
        (terminated (format nil "~A." s)))
    (loop
       for pc = "" then c
       for c across terminated
       do
         (push current uniform)
         (if (eql c pc)
             ;; then
             (setf current (cons c current))
             ;; else
             (setf current (list c)))
       finally
         (return uniform))))

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
  (let* ((s (read-line stream))
        (n (parse-integer (read-line stream)))
        (vals (loop for cc in (uniform-strings s) collect (weight-of-characters cc))
          ))
    (loop for nx from 1 to n
       do
         (format t
                 "~A~%"
                 (if (member
                      (parse-integer (read-line stream))
                      vals)
                     "Yes"
                     "No")))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
