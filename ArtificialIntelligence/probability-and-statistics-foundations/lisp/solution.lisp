;;; not applicable

(defun my-random ()
  (let* ((upto 5)
        (limit 1)
        (upper 100)
        (random-result
         (count-if (lambda (x) x)
                   (map 'list (lambda (x) (>= x limit))
                        (loop for x from 1 to upper collect (1+ (random upto)))))))
    (* 1.0 (/ upper random-result ))
    ))
