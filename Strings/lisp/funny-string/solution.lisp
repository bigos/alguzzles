(defun funny-p (str)
  (let ((n (length str))
        (rev (reverse str)))
    (every (lambda (x) x)
     (loop for i from 1 to (1- n)
        collect
          (eq
           (abs (- (char-code (elt str i))
                   (char-code (elt str (1- i)))))
           (abs (- (char-code (elt rev i))
                   (char-code (elt rev (1- i))))))))))

(defun solution (&optional stream)
  (let ((n (parse-integer (read-line stream))))
    (dotimes (x n)
      (princ (if (funny-p (read-line stream))
                 "Funny"
                 "Not Funny"))
      (terpri))))

;; (solution) ; uncomment this when running on hacker-rank

;;; still need to add  removing vertices
(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "Strings/lisp/funny-string/"
                                    "input0.txt"))
      (solution s))))
