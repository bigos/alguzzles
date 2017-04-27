;;; number whose divisors lead to efficient search for the solution.
(defvar *secret* 2308146389451302400)

(defparameter *table*
  '((1 . 1) (2 . 2) (3 . 6) (4 . 12) (5 . 60) (6 . 60) (7 . 420) (8 . 840)
   (9 . 2520) (10 . 2520) (11 . 27720) (12 . 27720) (13 . 360360) (14 . 360360)
   (15 . 360360) (16 . 720720) (17 . 12252240) (18 . 12252240) (19 . 232792560)
   (20 . 232792560) (21 . 232792560) (22 . 232792560) (23 . 5354228880)
   (24 . 5354228880) (25 . 26771144400) (26 . 26771144400) (27 . 80313433200)
   (28 . 80313433200) (29 . 2329089562800) (30 . 2329089562800)
   (31 . 72201776446800) (32 . 144403552893600) (33 . 144403552893600)
   (34 . 144403552893600) (35 . 144403552893600) (36 . 144403552893600)
   (37 . 5342931457063200) (38 . 5342931457063200) (39 . 5342931457063200)
   (40 . 5342931457063200)))

;;; trying to reduce the candidates for subsequent divisors
;; (let ((zz))
;;   (loop for x in (subseq *secret-divisors* 0 )
;;      for y =  (subsequent-numbers (divisors x))
;;      do
;;        (when (> (length y) (length zz))
;;          (format t "~&~A~%" (list x y))
;;          (push (list x  (reverse y) ) zz)))
;;   (format t "---------------~%")
;;   zz)

(defun small-divisors (n)
  (reverse
   (loop for d from 1 to (sqrt n)
      when (zerop (mod n d)) collect d)))

(defun divisors (n)
  (let* ((sd (small-divisors n))
         (rd (if (eq (expt (car sd) 2) n)
                 (cdr sd)
                 sd)))
    (loop for x in sd do
         (push (/ n x) rd))
    (reverse   rd)))

(defun subsequent-numbers (nl &optional acc)
  (if (and (cdr nl)
           (eq (1+ (car nl))
               (cadr nl)))
      (subsequent-numbers (cdr nl) (push (car nl) acc))
      (cons (car nl)
            acc)))

(defun range (ns ne)
  (loop for x from ns to ne collecting x))

(defun almost-subsequent (nl &optional acc missing)
  (cond ((and (cdr nl)
              (eq (1+ (car nl))
                  (cadr nl)))
         (almost-subsequent (cdr nl) (push (car nl) acc) missing))
        ((and (cdr nl)
              (null  missing)
              (eq (+ 2 (car nl))
                  (cadr nl)))
         (almost-subsequent (cdr nl) (push (car nl) acc) (1+ (car nl))))
        (t (list 'missing missing (reverse (cons (car nl)
                                                 acc))))))
(defun zzz (ns)
  (let* ((nn  (apply '* ns))
         (nnd (divisors nn)))
      (list 'divisors nn nnd 'almost (almost-subsequent nnd))    ))

(defun seek (n)
  (loop for x from 1 to (expt 10 6)
     until (>= (length (subsequent-numbers (divisors x)))
               n)
     finally (return x)))

(defun solve-me (n)
  (format t "~&~a"
          (loop for e in *table*
             until (eq (car e) n)
             finally (return (cdr e)))))

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
    (loop for x from 1 to tc do (solve-me (parse-integer (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
