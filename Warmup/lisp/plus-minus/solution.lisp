(defun fract (x n)
  (format nil "~,6f" (/ x n)))

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
  (let ((n (parse-integer (read-line stream)))
        (ints (split-and-parse (read-line stream)))
        (pos 0)
        (neg 0)
        (zer 0))
    (loop for x in ints do
         (if (= x 0)
             (incf zer)
             (if (< x 0)
                 (incf neg)
                 (incf pos)) ))
    (format t "~A~%~A~&~A~%"
            (fract pos n)
            (fract neg n)
            (fract zer n))))

;; (solution) ; uncomment this when running on hacker-rank

;;; still need to add  removing vertices
(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "Warmup/lisp/plus-minus/"
                                    "input0.txt"))
      (solution s))))
