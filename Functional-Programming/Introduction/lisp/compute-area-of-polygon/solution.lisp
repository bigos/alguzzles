;; http://code.activestate.com/recipes/578047-area-of-polygon-using-shoelace-formula/

(defun points (l central-point first-point last-seen acc)
  ;; (format t "~&~A : ~A ~A ~A : ~A~%" l central-point first-point last-seen acc)
  (if (null l)
      (apply #'+ acc)
      (points (cdr l)
              central-point
              (if first-point
                  first-point
                  (car l))
              (car l)       ;; last seen
              (if last-seen ;; acc
                  (cons (triangle-area (car l) last-seen first-point)
                        acc)
                  '()))))

;;; now i need area of triangle from the coordinates
(defun triangle-area (a b c)
  ;; (format t "~&a ~A b ~A c ~A~%" a b c)
  (abs (/ (+ (* (car a)
                (- (cadr b) (cadr c)))
             (* (car b)
                (- (cadr c) (cadr a)))
             (* (car c)
                (- (cadr a) (cadr b))))
          2.0)))

(defun find-central-point (l)
  (let ((min-x (apply 'min (map 'list 'car l)))
        (max-x (apply 'max (map 'list 'car l)))
        (min-y (apply 'min (map 'list 'cadr l)))
        (max-y (apply 'max (map 'list 'cadr l)))
        (mid-x)
        (mid-y))
    (setf mid-x (floor (/ (+ min-x max-x) 2))
          mid-y (floor (/ (+ min-y max-y) 2))
          )
    (format t "~&~A --- central point data~%" l)
    (list mid-x mid-y)))

(defun triangle-points (l acc)
  (points (cddr l) (find-central-point l) (car l) (nth 1 l) acc))

(defun solve-me (l)
  (triangle-points l nil))

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
  (let ((n (parse-integer (read-line stream))))
    (format t "~A~%" (solve-me (loop for x from 1 to n
                                  collect
                                    (split-and-parse (read-line stream)))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input1" :type "txt"))
    (solution s)))

(main)
