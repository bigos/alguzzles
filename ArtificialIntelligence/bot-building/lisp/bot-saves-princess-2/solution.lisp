(defun nextmove (n r c grid)
  (declare (ignore n))
  (labels
      ((position-difference (bot princess)
         (list (- (car princess)
                  (car bot))
               (- (cadr princess)
                  (cadr bot))))


       (princess-position (grid )
         (loop
            for r = 0 then (1+ r)
            for rd in grid
            for pp = (loop
                        for c = 0 then (1+ c)
                        for cd in rd
                        until (eq cd #\p)
                        finally (return (when (eq cd #\p) (list r c))))
            until pp
            finally (return pp)))
       (direction (vector)
         (let ((xd (first vector))
               (yd (second vector)))
           (cond
             ((< yd 0) "LEFT")
             ((> yd 0) "RIGHT")
             ((< xd 0) "UP")
             ((> xd 0) "DOWN")
             (T (error "no options left"))))))
    (let*
        ((princess-position (princess-position grid))
         (princess-vector (position-difference (list r c) princess-position)))
      (format t "~A~%"
              (direction princess-vector)))))


(defun solve-me (n grid bot-position)
  (next-move n
             (first bot-position)
             (second bot-position)
             grid))

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
         (bot-position (split-and-parse (read-line stream)))
         (grid (loop for x from 0 below n collect (split-string n (read-line stream)))))
    (solve-me n grid bot-position)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input1" :type "txt"))
    (solution s)))

(main)
