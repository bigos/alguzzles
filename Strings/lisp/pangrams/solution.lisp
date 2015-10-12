(defun letters-used (string)
  (let ((arr-char (make-array 128)))
    (loop for x from 0 below (length string)
       with c
       do
         (setf c (elt string x))
         (setf (elt arr-char (char-code c)) c))
    arr-char))

(defun pangram-p (string)
  (let ((letters (letters-used string)))
    (every (lambda (x) x)
           (loop for x from (char-code #\a) to (char-code #\z)
              collect (or (not (numberp (elt letters x)))
                          (not (numberp (elt letters (char-code (char-upcase (code-char x)))))))))))

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
