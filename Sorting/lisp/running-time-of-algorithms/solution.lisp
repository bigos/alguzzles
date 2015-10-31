(defun solve-me (n a)
  ;; (format t "~A ~A~%" n a)
  (let ((swapcount 0))
    (labels
        ((gteq (x)
           ;; (format t "~&********* ~A~%" x)
           (if (or (> x 0)
                   (< (1+ x) (- n 2)))
               (<= (aref a x) (aref a (1+ x)))
               T))
         (swap-vals (x)
           ;;(format t "~&swapping ~A ~A~%" x (1+ x))
           (incf swapcount)
           (let ((tv))
             (setf tv (aref a x))
             (setf (aref a x) (aref a (1+ x)))
             (setf (aref a (1+ x)) tv))))
      (loop for x = 0 then (if (gteq x)
                               (+ x 1)
                               (+ x 0))
         while (< x (1- n))
         do (unless (gteq x) (progn (swap-vals x)
                                    (when (> x 0)
                                      (setf x (1- x)))))))
    swapcount))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (let* ((n (parse-integer (read-line stream)))
        (a (make-array n :initial-contents (split-and-parse (read-line stream)))))
    (princ (solve-me n a))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
