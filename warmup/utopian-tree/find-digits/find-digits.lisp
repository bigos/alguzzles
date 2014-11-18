(defun task (num)
  (let* ((parsed (parse-integer num))
         (split-and-parsed (map 'list
                                (lambda (x)
                                  (parse-integer (format nil "~A" x)))
                                num)))
    (loop for n in split-and-parsed
       count (if (eq n 0)
                 nil
                 (zerop (mod parsed n)))
       into counted
       finally (return counted))))

(defun solution (&optional stream)
  (let* ((test-cases (parse-integer (read-line stream)))
         (nums (loop repeat test-cases
                  collect (read-line stream))))
    (loop for num in nums do
         (format t "~A~%" (task num)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "find-digits.input.1.txt"))
      (solution s))))

(repl-main)
