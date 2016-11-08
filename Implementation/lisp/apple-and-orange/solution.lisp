(defun summed (ds lr rr)
  (length
   (loop for d in ds
      when (and (>= d lr)
                (<= d rr))
      collect 1)))


(defun solve-me (s tt a b m n ma-ds nb-ds)
  (declare (ignore m n))
  (let ((alr (- s a))
        (arr (- tt a))
        (blr (- 0 (- b s)))
        (brr (- 0 (- b tt))))
    (format t "~A~%" (summed ma-ds alr arr))
    (format t "~A~%" (summed nb-ds blr brr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((st (split-and-parse (read-line stream)))
        (ab (split-and-parse (read-line stream)))
        (mn (split-and-parse (read-line stream)))
        (ma-ds (split-and-parse (read-line stream)))
        (nb-ds (split-and-parse (read-line stream))))
    (solve-me (car st)
              (cadr st)
              (car ab)
              (cadr ab)
              (car mn)
              (cadr mn)
              ma-ds
              nb-ds)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
