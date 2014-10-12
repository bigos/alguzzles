(defun string-to-char (str)
  (map 'list (lambda (x) x) str))

(defun counts (a uniques)
  (let (counts)
    (loop for u in uniques do
         (push (count u a ) counts))
    counts))

(defun solution (&optional stream)
  (let* ((a (string-to-char (read-line stream)))
         (uniques (remove-duplicates  a))
         (cts (counts a uniques))
         (odd-cts 0))
    (format nil "~A" a)
    (format nil "~a" cts)
    (loop for c in cts
       until (> odd-cts 1)
       do (when (oddp c)
            (incf odd-cts)))
    (format t "~a" (if (> odd-cts 1) "NO" "YES"))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "game-of-thrones-1.input1.txt"))
      (solution s)
      (format t "~&------------~%"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "game-of-thrones-1.input2.txt"))
      (solution s)
      (format t "~&------------~%"))))

(repl-main)
