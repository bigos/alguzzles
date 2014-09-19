(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun idea (x m)
  (+
   (* m
      (floor (/ (1- x) (1- m))))
   (if (zerop (mod x 3)) 3 (mod x 3)))
  )

(defun task (n c m)
  (let* ((bought (floor (/ n c)))
         (offer (floor (/ bought m)))
         (rest (mod bought m))
         (rest-used (if (zerop rest) 0 (/ offer rest))))
    (princ (list bought m (list offer) (list rest) rest-used))
    (terpri)
    (format t "~A~%" (+ bought offer rest-used))))

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
