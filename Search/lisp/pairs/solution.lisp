(defun solve-me (n k set)
  (let ((set2 (loop for x in set collecting (+ k x)))
        (both)
        (both2))
    (setf both (concatenate 'list set set2))
    (setf both2 (remove-duplicates both))
    (-  (length both) (length both2))))

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
  (let ((l1 (split-and-parse (read-line stream)))
        (l2 (sort (split-and-parse (read-line stream)) #'<)))
    (princ (solve-me (car l1) (cadr l1) l2))))

 ;; (solution) ; uncomment this when running on hacker-rank

;;; still need to add  removing vertices
(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "Search/lisp/pairs/"
                                    "input10.txt"))
      (solution s))))
