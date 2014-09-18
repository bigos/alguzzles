(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun cut-sticks (sticks)
  (let ((shortest))
    (loop do
         (progn
           (setf shortest (loop for s in sticks
                             minimizing s))
           (setf sticks (remove 0
                                (map 'list
                                     (lambda (x) (- x shortest))
                                     sticks)))
           (format t ">> ~A~%" sticks))
       while sticks)
    ))

(defun solution (&optional stream)
  (let* ((sticks-no (parse-integer (read-line stream)))
         (sticks (split-and-parse (read-line stream)))
         (cut-sticks sticks)
         )))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "cut-the-sticks.input1.txt"))
      (solution s))))

(repl-main)
