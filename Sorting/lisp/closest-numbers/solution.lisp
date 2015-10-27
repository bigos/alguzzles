(defun current-gap (n1 n2)
  (abs (- n1 n2)))

(defun solve-me (n a)
  (let* ((ar (make-array n :initial-contents (sort a '<)))
         (smallest-gap (abs (- (aref ar (1- n))
                               (aref ar 0))))
         (smallest-gap-pairs)
         (current-gap)
         (prev-num (* 1 smallest-gap))
         (current-num))
    (format t "~A ~A ~A" n ar smallest-gap)
    (loop for x from 1 below n do
         (setf current-num (aref ar x))
         (setf current-gap (current-gap prev-num current-num))
         (when (< current-gap smallest-gap)
           (setf smallest-gap current-gap
                 smallest-gap-pairs nil))
         (when (eq smallest-gap current-gap)
           (push (list prev-num current-num) smallest-gap-pairs))
         (format t "~&~A ~A ~A ~A ~A~%"
                 current-num
                 prev-num
                 smallest-gap
                 current-gap
                 smallest-gap-pairs)
         (setf prev-num current-num))
    (format t "~&===========================~%")
    (loop for p in smallest-gap-pairs
       for leading-spaces = "" then " "
       do
         (format T "~A~A ~A" leading-spaces (car  p) (cadr p)))
    ))


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
  (let ((n (parse-integer (read-line stream)))
        (a (split-and-parse (read-line stream))))
    (solve-me n a)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
