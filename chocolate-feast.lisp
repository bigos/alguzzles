(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun task (n c m)
  (let* ((bought (floor (/ n c)))
         (offer (floor (/ bought m)))

         )
    (terpri)))

(defun solution (&optional stream)
  (let* ((tests (parse-integer (read-line stream)))
         (ncms (loop repeat tests
                  collect (split-and-parse (read-line stream)))))
    (loop for z in ncms do
         (task (nth 0 z)
               (nth 1 z)
               (nth 2 z)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "chocolate-feast.1.txt"))
      (solution s))))

(repl-main)
