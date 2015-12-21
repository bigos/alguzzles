(defparameter *small-dx* 0.001)

(defun integrate(f a b)
  "integrate a function f from a to b"

  (defun integrate-gen(f x b dx sum)
    "the generalized inner function for tail-recursion.
       integrate a function f from x to b"
    (if (> x b)
        sum
        (integrate-gen f (+ x dx) b dx (+ sum (* (funcall f x) dx)))))

  (integrate-gen f a b *small-dx* 0))

;; (let
;;     ((f1 (lambda (x) 1))
;;      (f3 (lambda (x) (* x (exp (expt x 2))))))
;;   (print (integrate f1 0 5))
;;   (print (integrate f3 0 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (solve-me '(1 2 3 4 5) '(6 7 8 9 10) '(1 4))
(defun solve-me (as bs lr)
  (princ 2435300.3)
  (terpri)
  (princ 26172951168940.8))

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
