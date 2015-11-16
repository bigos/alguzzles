;;; to see compiler notes run
;;; M-x slime-compile-file in the Emacs buffer
;;; slime-compile-defun with a cursor inside of a defun

(defun max-subarray (ints n m)
  ;; (declare (optimize (speed 3)))
  (let ((indexes (make-array (list n)))
        (my-max 0))
    (loop
       for x in ints
       for i = 0 then (1+ i)
       for y = (car ints) then (+ x y)
       do
         (setf (aref indexes i) (mod y m)))
    (loop for s from 0 below n do
         (loop for x from s below n do
              (setq my-max (max my-max
                                (mod
                                 (if (zerop s)
                                     (aref indexes x)
                                     (- (aref indexes x)
                                        (aref indexes (1- s))))
                                 m)
                                ))))
    my-max))

(defun solve-me (nm ints)
  (princ (max-subarray ints (car nm) (cadr nm)))
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
  (let ((tc (parse-integer (read-line stream))))
    (dotimes (x tc)
      (solve-me (split-and-parse (read-line stream))
                (split-and-parse (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input07" :type "txt"))
    (solution s)))

(main)
