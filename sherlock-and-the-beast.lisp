(defun binary-to-decent (bin len)
  (let* ((nstr (substitute #\5
                           #\1
                           (substitute #\3
                                       #\0
                                       (format nil
                                               (format nil "~~~d,'0b" len)
                                               bin) )))
         (count3 (count #\3 nstr))
         (count5 (count #\5 nstr)))
    (if (and (= (+ count3 count5)
                (length nstr))
             (zerop (rem count3 5))
             (zerop (rem count5 3)))
        (parse-integer nstr)
        nil)))

(defun largest-decent (len)
  (let ((res))
    (loop for bin from (expt 2 len) downto 0
       do (setf res (binary-to-decent bin len))
       until res
       finally (return res))))

(defun solution (&optional stream)
  (let* ((tc (parse-integer (read-line stream)))
         (nums (loop repeat tc collect (parse-integer (read-line stream)))))
    (loop for x in nums
       with r = (largest-decent x)
       do (format t "~A~%" (if r r -1)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "sherlock-and-the-beast.input.1.txt"))
      (solution s))))

(repl-main)
