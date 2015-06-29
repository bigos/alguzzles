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
  (let* ((test-cases (parse-integer (read-line stream))) (zzz)
         (ladders) (ll)
         (snakes) (ss))
    (format t "~A~%" test-cases)
    (dotimes (tc test-cases )
      (format t " --------------- ~%")
      (setq ladders (parse-integer (read-line stream)))
      (format t "~a ladders ~%" ladders)
      (setq ll (loop for l from 0 below ladders
                  collecting (split-and-parse (read-line stream))))
      (setq snakes (parse-integer (read-line stream)))
      (format t "~a snakes ~%" snakes)
      (setq ss (loop for s from 0 below snakes
                  collecting (split-and-parse (read-line stream))))
      (setq zzz   (nconc zzz (list (list  ll  ss)))))
    (format t "qqqqqqqqqqqq ~A~%"  zzz)  ))

 ;; (solution) ; uncomment this when running on hacker-rank

;;; still need to add  removing vertices
(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "GraphTheory/lisp/snakes-and-ladders/"
                                    "input0.txt"))
      (solution s))))
