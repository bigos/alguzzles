(proclaim '(optimize (speed 3) (safety 0)))

(defun powers-of-2 (n)
  (reverse
   (subseq
    (reverse
     (loop for p from 0 to 20
        collect p
        until (>= (expt 2 p) n)))
    0 2)))

(defun counts (m n)
  (let ((nums))
    (loop for x from m to n by 1 do
         (setf nums
               (loop for y from 0 to x
                  when (eq (+ x y)
                           (logxor x y))
                  collect y))
         (format t "~6,b ~3,d ~3,d ~6,b ~a     ~a~%"
                 x
                 x
                 (length nums)
                 (length nums)
                 (loop for p from 0 until (eq (expt 2 p)
                                              (length nums))
                    finally (return p))
                 (find-num x)))))

(defun find-num (n)
  (let ((range-powers (powers-of-2 n)))
    (if  (eq n (expt 2 (cadr range-powers)))
         n
         (binary-find-num n
                          (1- (car range-powers))
                          (- (expt 2 (cadr range-powers))
                             (expt 2 (1- (car range-powers))))
                          (1- (car range-powers))))
    ))

(defun binary-find-num (n pp pv  up )
  (if (eq pv n)
      (expt 2 pp)
      (binary-find-num n
                       (if (< n pv)
                           (- pp 0)
                           (- pp 1))
                       (funcall (if (< n pv) #'- #'+) pv (expt 2 (1- up)))
                       (1- up))))

(defun solve-me (n)
  (princ (find-num n))
  (terpri))

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
                      :name "input07" :type "txt"))
    (solution s)))

(main)
