(defun solution (&optional stream)
  (let ((n (parse-integer (read-line stream))))
    (loop for c from 1 to n
       for s from (1- n) downto 0
       do
         (dotimes (x s) (princ #\space))
         (dotimes (x c) (princ #\#))
         (terpri))))

;; (solution) ; uncomment this when running on hacker-rank

;;; still need to add  removing vertices
(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "Warmup/lisp/staircase/"
                                    "input0.txt"))
      (solution s))))
