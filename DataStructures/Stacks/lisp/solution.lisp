(defparameter brackets
  '( (#\( . #\)) (#\{ . #\}) (#\[ . #\])))

(defun balanced? (str &optional stack)
  (if (zerop (length str))
      (zerop (length stack))
      (let ((c (car (member (car str)
                            brackets :key #'cdr))))
        (if c
            (if
             (equalp (car c)
                     (car stack))
             (balanced? (subseq str 1)
                        (cdr stack))
             nil)
            (balanced? (subseq str 1)
                       (cons (car str)
                             stack ))))))

(defun solve-me (str)
  (format t "~A~%" (if (balanced? str)
                       "YES"
                       "NO")))

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

(defun string-to-characters (string)
  (loop for c across string
     collect c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((n (parse-integer (read-line stream))))
    (dotimes (x n)
      (solve-me (string-to-characters (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
