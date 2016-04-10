;; CL-USER> (vals 11 5 5)
;; (5 15 30 50 75 105 140 180 225 275 330)
;; CL-USER> (vals 12  3 2)
;; (3 8 15 24 35 48 63 80 99 120 143 168)
;;; if i could substract bottom left eg. 15 - 3 or 30 - 8
;; I could get the desired values
;;;  now the problem is how to write a function that would calculate the value
;; without accum

(defun val2 (n)
  (* (+ 2 n) n))

(defun val5 (n)
  (* (* 2.5 (1+ n)) n))

(defun solve-me-too (n)
  (cond ((eq n 1) 1)
        ((eq n 2) 5)
        (T (- (val5 (1- n)) (val2 (- n 2))))))

(defun solve-me (n)
  (loop for s from 1 to n
     for x = 1 then (+ x 5)
     for y = 0 then (+ y (if (eq x 1) 0 2))
     sum (if (eq x 1) 1  (- (1- x) (1- y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (loop for n from 1 to tc do
         (princ
          (solve-me
           (parse-integer (read-line stream))))
         (terpri))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
