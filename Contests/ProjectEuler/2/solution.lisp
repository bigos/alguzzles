;;;

(defparameter limit (* 4 (expt 10 16)))

(defun fib (n a)
  (if (or (> (car a) n)
          (> (car a) limit))
      (cdddr (reverse (cdr a)))
      (fib n (cons (+ (car a) (cadr a)) a))))

(defparameter all-fib (fib limit '(0 1)))
(defparameter even-fib (remove-if-not #'evenp all-fib))

(defun zzz (l a)
  (if (zerop (length l))
      (reverse a)
      (zzz (cdr l)
           (cons (+ (car l) (car a)) a))))

(defparameter sums (map 'list (lambda (x y) (list x y))
                        even-fib (cdr (zzz even-fib '(0)))))



(defun hhh (n)
  (cadar (last (loop for a in sums while (<= (car a) n) collect a))))

(defun solve-me (n)
  (format T "~A~%" (hhh n)))

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
  (let ((tc (parse-integer (read-line stream))))
    (loop for x from 1 to tc do (solve-me (parse-integer (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
