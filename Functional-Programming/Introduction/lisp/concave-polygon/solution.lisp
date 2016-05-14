;;; concave
(defparameter zzz (solve-me '((-1 1) (0 2) (1 3) (3 3) (3 2) (3 -1) (2 -1) (1 -1) (1 1))))
;;; convex
(defparameter aaa (solve-me '((-1 1) (0 2) (1 3) (3 3) (3 2) (3 -1) (2 -1) (1 -1) (-1 0))))

;;; (loop for x in zzz do (format t "~A ~A ~A~%" x (my-test x) (my-vectors x)))

(defun substract-vectors (a b)
  (list (- (car a) (car b))
        (- (cadr a) (cadr b))))

(defun my-vectors (points)
  (list
   (substract-vectors (elt points 1)
                      (elt points 0))
   (substract-vectors (elt points 2)
                      (elt points 0))))
(defun rtd (r)
  (* r (/ 180.0 pi)))

(defun vector-dot (a b)
  (+ (* (car a) (car b))
     (* (cadr a) (cadr b))))

(defun vector-dziel (a b)
  (* (sqrt (+ (expt (car a) 2)
              (expt (cadr a) 2)))
     (sqrt (+ (expt (car b) 2)
              (expt (cadr b) 2)))))

;;; i have problem getting expected vector angle in degrees
;; radians to degrees seems to work
;; the formula seems to work
;; but the degree angles are wrong, why?

(defun my-test (points)
  (let* ((res (my-vectors points))
         (v1 (car res))
         (v2 (cadr res))
         (dot (vector-dot v1 v2))
         (dziel (vector-dziel v1 v2)))
         (format t "~A ~A ~A ~A ~A~%" v1 v2 dot dziel (/ dot dziel))
    (list 'aaaaa dot  dziel (rtd (cos (/ dot dziel))) 'zzzz)))

(defun points (l last-point before-last-point acc two-last-points)
  (if (null l)
      acc
      (points (cdr l)
              (car l)                   ;future last
              last-point
              (cons (list (car l)
                          (if last-point
                              last-point
                              (cadr two-last-points))
                          (if before-last-point
                              before-last-point
                              (if last-point
                                  (cadr two-last-points)
                                  (car two-last-points))))
                    acc)
              two-last-points)))

(defun solve-me (l)
  (format t "~A~%" l)
  (points l nil nil nil (subseq l (- (length l) 2))))

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
