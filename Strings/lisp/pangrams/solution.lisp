(defun pangram-p (string)
  (let ((todo))
    nil))

(defun solution (&optional stream)
  (let* ((test-string (read-line stream)))
    (princ  (if (pangram-p test-string)
                "pangram"
                "not pangram"))))

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
                                    "input3.txt"))
      (solution s))))
