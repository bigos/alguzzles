;;; i forgot to sort the points before evaluation

(defun rotate-vector-90deg-ccw (a)
  (list (- (cadr a)) (car a)))

(defun substract-vectors (a b)
  (list (- (car a) (car b))
        (- (cadr a) (cadr b))))

(defun add-vectors (a b)
  (list (+ (car a) (car b))
        (+ (cadr a) (cadr b))))

(defun my-vectors (points)
  (list
   (substract-vectors
    (elt points 0)
    (elt points 1))
   (substract-vectors
    (elt points 2)
    (elt points 1))))

(defun rtd (r)
  (* r (/ 180.0 pi)))

(defun vector-dot (a b)
  (+ (* (car a) (car b))
     (* (cadr a) (cadr b))))

(defun vector-dziel (a b)
  ;; (format tg "~A ~A~%" a b)
  (* (sqrt (+ (expt (car a) 2)
              (expt (cadr a) 2)))
     (sqrt (+ (expt (car b) 2)
              (expt (cadr b) 2)))))

(defun vector-angle (a b)
  (rtd (acos (/ (vector-dot a b)
                (vector-dziel a b)))))

(defun my-test (points)
  (let* ((res (my-vectors points))
         (v1 (car res))
         (v2 (cadr res))
         (v1-rotated (rotate-vector-90deg-ccw v1))
         (v2-rotated (rotate-vector-90deg-ccw v2))
         (dot (vector-dot v1 v2))
         (dziel (vector-dziel v1 v2))
         (v1-rot-and-v2 (vector-angle v1-rotated v2)))
    (format t "~a / ~A ~A  ~A ~A   angle v1v2 ~A other ~A ~A  = ~A~%"
            points v1 v2
            v1-rotated
            v2-rotated
            (vector-angle v1 v2)
            (vector-angle v1-rotated v2)
            (vector-angle v2-rotated v1)

            (if (< (if (not (realp v1-rot-and-v2))
                       (realpart v1-rot-and-v2)
                       v1-rot-and-v2)
                   90)
                'less
                'more)
            )
    (rtd (acos (/ dot dziel))) ))

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
  (let ((result))
    ;; (format t "~A~%" l)
    (setf result
          (loop for x in
               (points l nil nil nil (subseq l (- (length l) 2)))
             sum (my-test x)))
      (format t "~A~%" result)
     (if (< (mod result 360) 5)
        "YES"
        "NO")))

;;; concave
;;; (defparameter zzz (solve-me '((-1 1) (0 2) (1 3) (3 3) (3 2) (3 -1) (2 -1) (1 -1) (1 1))))
;;; convex
;;; (defparameter aaa (solve-me '((-1 1) (0 2) (1 3) (3 3) (3 2) (3 -1) (2 -1) (1 -1) (-1 0))))

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
                      :name "input03" :type "txt"))
    (solution s)))

(main)
