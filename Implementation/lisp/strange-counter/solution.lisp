(defparameter *max-val* (* 5 (expt 10 12)))

(defun tops (n tx v res)
  (cond ((> tx *max-val*) 'error)
        ((>= tx n) (push (cons tx v) res))
        (T (tops n (+ tx v) (* v 2) (push (cons tx  v) res)))))

(defun more (n tv)
  (- (cdr tv)
     (- n (car tv))))

(defun solve-me (n)
  (let ((my-tops (tops n 1 3 nil)))
    (format t "~A~%" (if (eq (caar my-tops) n)
                         (cdar my-tops)
                         (more n (cadr my-tops))))))

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
  (let ((n (parse-integer (read-line stream))))
    (solve-me n)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
