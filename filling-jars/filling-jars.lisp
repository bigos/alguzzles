(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun solution (&optional stream)
  (let* ((nm (split-and-parse (read-line stream)))
         (data (loop repeat (cadr nm)
                  collect (split-and-parse (read-line stream)))))
    (format t "~A~%"
            (floor  (apply #'+
                           (loop for d in data
                              for res = (* (caddr d) (1+ (- (cadr d) (car d))))
                              collect res))
                    (car nm)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/"))
        (puzzle "filling-jars"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" puzzle ".input.1.txt"))
      (solution s))))

(repl-main)
