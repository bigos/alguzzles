(defun steps (distance direction)
  (list
   (cons distance (dir (cadr direction) T))
   (cons distance (dir (car direction)  nil  ))))

(defun dir (n hor)
  (if hor
      (if (> n 0) 'right 'left)
      (if (> n 0) 'down   'up)))

(defun solve-2 (n princess)
  (let* ((distance (floor (/ n 2)))
        (me (list distance distance))
        (direction)
        (steps)
        (vd)
        (hd))

    (setf direction (list (- (car princess)
                             (car me))
                          (- (cadr princess)
                             (cadr me))))
    (setf steps (steps distance direction))
    (setf vd (cdar steps))
    (setf hd (cdadr steps))
    ;; (format t "princess location~%~A ~A ~A ~A ~A ~A~%" princess me direction steps vd hd)
    (loop for x from 0 below distance do (format t "~A~%~A~%" vd hd))))

(defun solve (n grid)
  (let ((princess))
    (loop
       for r from 0 below n
       for l in grid do
         (loop
            for c from 0 below n
            for ch in l do
              (when (eq ch #\p)
                (setf princess (list r c)))))
    (solve-2 n princess)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-string (length string)
  (loop for i from 0 below length collect (char string i )))

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
        (grid (loop for x from 0 below n collect (split-string n (read-line stream)))))
    (solve n grid)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
