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
         (format t "~6,b ~3,d ~3,d ~6,b ~a~%"
                 x
                 x
                 (length nums)
                 (length nums)
                 (loop for p from 0 until (eq (expt 2 p)
                                              (length nums))
                    finally (return p))))))
(defun find-num (n)
  (let ((range-powers (powers-of-2 n)))
    (cond ((eq n (expt 2 (cadr range-powers)))
           n)
          (T
           (binary-find-num n
                            (car range-powers)
                            (cadr range-powers)
                            (1- (car range-powers))
                            (- (expt 2 (cadr range-powers))
                               (expt 2 (1- (car range-powers)))))))))

(defun binary-find-num (n ps pe pp pv)
  (format t "~A ~A ~A ~A ~a~%" n ps pe pp pv)
  (if (eq pv n)
      (expt 2 pp)
      (if (< n pv)
          (binary-find-num n ps pe pp (- pv (expt 2 (1- pp))))
          (binary-find-num n ps pe (1- pp) (+ pv (expt 2 (1- pp))))
          )))

(defun solve-me (n)
  (loop for x from 0 to 500 do
       (when (eq (+ n x)
                 (logxor n x))
         (incf *cnt*)))
  (princ *cnt*)
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
                      :name "input0" :type "txt"))
    (solution s)))

(main)
