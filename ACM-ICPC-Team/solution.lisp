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

(defun known-by-team (x y topics topic-data)
  (loop for topic from 0 below topics
     summing (if (or (eq (elt (elt topic-data x) topic) #\1)
                     (eq (elt (elt topic-data y) topic) #\1))
                 1
                 0)))

(defun puzzle (people topics topic-data)
  (let ((results) (max-result) (max-count))
    (loop for x from 0 below people
       do
         (loop for y from (1+ x) below people
            do (push (known-by-team x y topics topic-data) results)))
    (setf max-result (apply #'max results ))
    (setf max-count (count max-result results))
    (format t "~A~%~A~%" max-result max-count)))

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
