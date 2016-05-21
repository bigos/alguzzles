;; http://code.activestate.com/recipes/578047-area-of-polygon-using-shoelace-formula/

(defun points (l first-point last-seen acc+ acc-)
  ;; (format t "~&~A : ~A : ~A  ~A~%" l last-seen acc+ acc-)
  (if (null l)
      (list (cons
             (list (cadr first-point) (car last-seen))
             acc+)
            (cons
             (list (car first-point) (cadr last-seen))
             acc-))
      (points (cdr l)
              (if first-point
                  first-point
                  (car l))
              (car l) ;; last seen
              (when last-seen
                (cons (list (car last-seen)
                            (cadr (car l)))
                      acc+))
              (when last-seen
                (cons (list (car (car l))
                            (cadr last-seen))
                      acc-)))))

(defun solve-me (l)
  (let ((data
         (points l nil nil nil nil)))
    (/ (abs (+ (apply '+ (map 'list (lambda (x) (* (car x) (cadr x)))
                              (car data)))
               (apply '+ (map 'list (lambda (x) (- 0 (* (car x) (cadr x))))
                              (cadr data)))))
       2.0)))

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
                      :name "input0" :type "txt"))
    (solution s)))

(main)
