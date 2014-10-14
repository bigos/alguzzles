(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun split-string-to-chars (str)
  (map 'list #'character str))

(defun teams (count)
  (loop for x from 0 below count
     do
       (loop for y from (1+ x) below count
          do (format t "~A ~A~%" x y))))


(defun puzzle (people topics topic-data)
  (format t "~A ~A ~s~%" people topics topic-data)
  (loop for t from 0 to (1- topics)
       ))

(defun solution (&optional stream)
  (let* ((nm (split-and-parse (read-line stream)))
         (data (loop repeat (car nm)
                  collect (split-string-to-chars (read-line stream)))))
    (puzzle (car nm) (cadr nm) data)))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/"))
        (puzzle "ACM-ICPC-Team"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input.1.txt"))
      (solution s))))

(repl-main)
