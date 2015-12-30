;;; -----------------------------------------------------------------------
(defparameter *small-dx* 0.001)

(defun integrate(f a b)
  (labels ((integrate-gen (f x b dx sum)
             (if (> x b)
                 sum
                 (integrate-gen f (+ x dx) b dx (+ sum (* (funcall f x) dx))))))
    (integrate-gen f a b *small-dx* 0)))

;; (solve-me '(1 2 3 4 5) '(6 7 8 9 10) '(1 4))

(defun solve-me (as bs lr)
  (princ (loop for x from (1- (car lr)) to (1- (cadr lr))
            ;; do
            ;;   (format t
            ;;           "~A ~A ~A - ~A ~A ~%"
            ;;           x
            ;;           (elt as x)
            ;;           (elt bs x)
            ;;           (cons (elt as x) (elt as (1+ x)))
            ;;           (elt bs x))

            sum (integrate (lambda (i) (expt i (elt bs x)))
                           (elt as x)
                           (elt as (1+ x))))))

;;; --------------------------------------------------------------------------

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
  (let ((as (split-and-parse (read-line stream)))
        (bs (split-and-parse (read-line stream)))
        (lr (split-and-parse (read-line stream))))
    ;;(format t "read ~A ~A ~S" as bs lr)
    (solve-me as bs lr)
    ))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
