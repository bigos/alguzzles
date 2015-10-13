(defun solve-me (m n)
  (princ (abs
          (- (loop for r from (1- n) downto 0
                for c from 0 below n
                summing (aref m r c))
             (loop for r from 0 below n
                for c from 0 below n
                summing (aref m r c))))))

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
  (let* ((n (parse-integer (read-line stream)))
         (m (make-array (list n n) :initial-element 0)))
    (loop for r from 0 below n do
         (loop for cc in (split-and-parse (read-line stream))
            for s = 0 then (1+ s) do
              (setf (aref m r s) cc)))
    (solve-me  m n)))

 ;; (solution) ; uncomment this when running on hacker-rank

;;; still need to add  removing vertices
(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "Warmup/lisp/diagonal-difference/"
                                    "input2.txt"))
      (solution s))))
