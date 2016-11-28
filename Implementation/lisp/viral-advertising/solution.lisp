(defun cc (m)
       (floor (/ (* m 3) 2)))

(defun recsol (n day res)
  (if (>= day n)
      res
      (+ res
         (recsol n (1+ day) (cc res )))))

(defun solve-me (n)
  (format t "~A~%" (recsol n 1 2)))
