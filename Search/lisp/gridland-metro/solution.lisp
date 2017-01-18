(declaim (optimize (speed 3)))

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
  (let* ((nmk (split-and-parse (read-line stream)))
         (n (nth 0 nmk))
         (m (nth 1 nmk))
         (k (nth 2 nmk))
         (rh (make-hash-table))
         (max-lamps (* n m))
         (tempar))
    (loop
       for l from 1 to k
       for rl = (split-and-parse (read-line stream))
       do
         (push (cdr rl)
               (gethash (car rl) rh)))
    (loop
       for r from 1 to n
       for rhv = (gethash r rh)
       do
         (when rhv
           (setf tempar (make-array (list m)
                                    :initial-element 1
                                    :element-type 'bit))
           (loop for rse in rhv
              do
                (loop for c from (1- (car rse)) below (cadr rse)
                   do
                     (unless (zerop (aref tempar c))
                       (setf (aref tempar c) 0
                             max-lamps (1- max-lamps)))))
           (format t "~s ~s~%" rhv tempar)
           ))

    (format t "~S~%"  max-lamps)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
