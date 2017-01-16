(defun compare-strings (a b i)
  (let* ((common-prefix-len
          (loop for c in (map 'list (lambda (x y)
                                      (cons x y))
                              a b)
             until (not (eql (car c) (cdr c)))
             count c))
         (min-len (abs (+ (- (length a) common-prefix-len)
                          (- (length b) common-prefix-len)))))
    (format nil " !!!!!!!!!! ~A !!! ~A~%" common-prefix-len min-len)

    (cond ((eql i min-len)
           ;; (princ 1)
           T)
          ((> i (+ (* 2 common-prefix-len) min-len))
           ;; (princ 2)
           T)
          ((and
            (> i min-len)
            (zerop (mod (- i min-len) 2)))
           ;; (princ 4)
           T)
          (T
           nil))))

(defun solve-me (ss st ik)
  (format t "~A~%" (if (compare-strings ss st ik)
                       "Yes"
                       "No")))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (let ((ss (read-line stream))
        (st (read-line stream))
        (ik (car (split-and-parse (read-line stream)))))
    (solve-me ss st ik)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
