(defun my-solution (&optional arg)
  (let* ((vt (parse-integer (read-line arg)))
         (lines (loop repeat vt collect (read-line arg))))
    (loop for l in lines
       do
         (format t "~A~%" (calc (parse-integer l))))))

(defun calc (i)
  (loop for x from 0 to i
     for total = 1 then (if (oddp x)
                            (* total 2)
                            (+ 1 total))
     finally (return total)))

(my-solution)
;; testing in repl
;; (my-solution (make-string-input-stream (format nil "2 ~%0~%1~%")))
