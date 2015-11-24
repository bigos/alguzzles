(defun f (n list)
  ;; Complete this function
  )
(defun read-list ()
  (let ((n (read *standard-input* nil)))
    (if (null n)
        nil
        (cons n (read-list)))))

(format t "~{~d~%~}" (f (read) (read-list)))
