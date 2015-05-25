;; (require :sb-sprof)

;; (declaim (optimize (speed 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *original-edges* nil)
(defparameter *forests* nil)
(defparameter *nodes* nil)

(defun initialize (edges)
  (format t "~%~A ~%" edges)
  (setq *original-edges* edges)
  (setq *forests* (connections edges))
  (setq *nodes* (nodes edges)))

(defun nodes (edges)
  (remove-duplicates
   (loop for edge in edges
      collect (car edge)
      collect (cdr edge))))

(defun connections (edges)
  (let ((neighbours) (found))
    (loop for edge in edges do
         (unless (setq found (position (car edge) neighbours :key 'car))
           (push `(,(car edge) (,(cdr edge))) neighbours)))
    (loop for edge in edges do
         (if (setq found (position (cdr edge) neighbours :key 'car))
             (unless (position (car edge) (cadr (elt neighbours found)))
               (push (car edge)  (cadr (elt neighbours found))))
             (push `(,(cdr edge) (,(car edge))) neighbours)))
    neighbours))

(defun connections-for (node connections)
  (loop for c in connections
     until (eq node (car c))
     finally (return (cadr c))))

(defun remove-connections (node connections)
  (remove-if (lambda (x) (eq (car x) node)) connections))

(defun append-connections (node appended-nodes connections)
  (loop for c in connections
     until (eq node (car c))
     finally (progn
               (setf (cadr c) (remove-duplicates
                               (concatenate 'list (cadr c) appended-nodes)))
               (return connections))))

(defun move-connections (from to connections)
  (let ((from-connections (connections-for from connections)))
    (unless (eq from to)
      (setf connections (append-connections to from-connections connections))
      (setq connections (remove-connections from connections)))
    ;; (format t "~&~A  ~A~%~A~%" from to from-connections )
    connections))

(defun connection-points (connections)
  (loop for cp in connections collecting (car cp)))

;;; match for the first connection
(defun find-matching-first (seek connections)
  (map 'list (lambda (x) (or (equalp (car x) seek)
                             (not (not (position seek (cadr x))))))
       connections))

(defun find-matching-rest (from seeks connections) ;stuck again
  (map 'list (lambda (x) (not (every #'null x)))
       (map 'list
            (lambda (x)
              (map 'list
                   (lambda (z) (position z seeks))
                   (cadr x)))
            (subseq connections from))))

(defun a2b (skip a b)
  (let ((old-forests *forests*))
    (setq *forests* (move-connections a b *forests*))
    (format t "~&~A ~A ~&~A~&~a~&~A~%"
            *forests*
            (equalp old-forests *forests*)
            (connection-points *forests*)
            (find-matching-first (caar *forests*) *forests*)
            (find-matching-rest 0 (cadar *forests*) *forests*)
            )))

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

(defun arx ()
  '((10 . 8) (9 . 8) (8 . 6) (7 . 2) (5 . 2) (4 . 3) (2 . 1)))

;;; two forests 6 8 9 10 & 1 2 3 4 5 7
(defun ary ()
  '((10 . 8) (9 . 8) (8 . 6) (7 . 2) (5 . 2) (4 . 3) (1 . 3) (2 . 1)))

(defun arr ()
  '((10 . 8) (9 . 8) (8 . 6) (7 . 2) (6 . 1) (5 . 2) (4 . 3) (3 . 1) (2 . 1)))

(defun arr2 ()
  '((30 . 25) (29 . 4) (28 . 27) (27 . 5) (26 . 17) (25 . 21)
    (24 . 12) (23 . 2) (22 . 20) (21 . 15) (20 . 4) (19 . 18)
    (18 . 17) (17 . 1) (16 . 10) (15 . 8) (14 . 2) (13 . 2) (12 . 8)
    (11 . 4) (10 . 4) (9 . 5) (8 . 1) (7 . 4) (6 . 4) (5 . 2) (4 . 3)
    (3 . 2) (2 . 1)))

;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let* ((dd (split-and-parse (read-line stream)))
         (n (car dd))
         (m (cadr dd))
         (ar))
    (declare (ignore n)) ;; ignore unused n
    (loop for x in
         (loop for row below m
            collecting (split-and-parse (read-line stream)))
       do (push (cons (car x) (cadr x)) ar))
    (format T "ar is: ~A~%" ar)
    (format t "~%finished~%" )))

 ;; (solution) ; uncomment this when running on hacker-rank


(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "GraphTheory/lisp/even-tree/"
                                    "input2.txt"))
      (solution s))))

;; using profiler
;; (sb-sprof:start-profiling)
;; (sb-sprof:with-profiling (:max-samples 1000 :report :flat :loop T :show-progress T)
;;   (repl-main))
;;(sb-sprof:stop-profiling)
;; (sb-sprof:report)


;;
;; 4
;; |
;; 3  5
;; +  |
;; 1--2
;; +  |
;; 6  7
;; |
;; 8--10
;; |
;; 9

;; (sb-sprof:start-profiling)
;; (sb-sprof:with-profiling
;;     (:max-samples 1000 :show-progress T :mode :cpu :report :flat)
;;   (dotimes (x 1)  (repl-main)))

;;; (sb-sprof:with-profiling (:max-samples 100 :mode :cpu :report :flat)  (repl-main))


;; obtaining optimisation notes
;; (compile-file "~/Programming/hackerrank/GraphTheory/lisp/even-tree/solution.lisp" )



;;; REPL

;; (load "/Users/jacekpodkanski/Documents/hackerrank/GraphTheory/lisp/even-tree/solution.lisp")

;; finished

;; T
;; CL-USER> (graph::initialize (ary))

;; ((10 . 8) (9 . 8) (8 . 6) (7 . 2) (5 . 2) (4 . 3) (1 . 3) (2 . 1))
;; (10 9 8 6 7 5 4 3 2 1)
;; CL-USER> (graph::a2b   3 4 )
;; ((6 (8)) (2 (5 7 1)) (1 (2 3)) (4 (3 1 4)) (5 (2)) (7 (2)) (8 (9 10 6)) (9 (8))
;;  (10 (8))) NIL
;; (6 2 1 4 5 7 8 9 10)
;; (T NIL NIL NIL NIL NIL 2 NIL NIL)
;; ((0) (NIL NIL NIL) (NIL NIL) (NIL NIL NIL) (NIL) (NIL) (NIL NIL NIL) (0) (0))
;; NIL
;; CL-USER> (graph::find-matching-rest 0 '(8) graph::*forests*)
;; ((0) (NIL NIL NIL) (NIL NIL) (NIL NIL NIL) (NIL) (NIL) (NIL NIL NIL) (0) (0))
;; CL-USER> (graph::a2b   6 10  )
;; ((2 (5 7 1)) (1 (2 3)) (4 (3 1 4)) (5 (2)) (7 (2)) (8 (9 10 6)) (9 (8))
;;  (10 (8))) NIL
;; (2 1 4 5 7 8 9 10)
;; (T 0 NIL 0 0 NIL NIL NIL)
;; ((0 1 2) (NIL NIL) (NIL 2 NIL) (NIL) (NIL) (NIL NIL NIL) (NIL) (NIL))
;; NIL
;; CL-USER> (graph::find-matching-rest 0 '(5 7 1) graph::*forests*)
;; ((0 1 2) (NIL NIL) (NIL 2 NIL) (NIL) (NIL) (NIL NIL NIL) (NIL) (NIL))
;; CL-USER> (graph::a2b   2 4  )
;; ((1 (2 3)) (4 (3 4 5 7 1)) (5 (2)) (7 (2)) (8 (9 10 6)) (9 (8)) (10 (8))) NIL
;; (1 4 5 7 8 9 10)
;; (T 4 NIL NIL NIL NIL NIL)
;; ((0 1) (1 NIL NIL NIL NIL) (0) (0) (NIL NIL NIL) (NIL) (NIL))
;; NIL
;; CL-USER> (graph::find-matching-rest 0 '(2 3) graph::*forests*)
;; ((0 1) (1 NIL NIL NIL NIL) (0) (0) (NIL NIL NIL) (NIL) (NIL))
;; CL-USER> (graph::a2b   1 4  )
;; ((4 (4 5 7 1 2 3)) (5 (2)) (7 (2)) (8 (9 10 6)) (9 (8)) (10 (8))) NIL
;; (4 5 7 8 9 10)
;; (T NIL NIL NIL NIL NIL)
;; ((0 1 2 3 4 5) (4) (4) (NIL NIL NIL) (NIL) (NIL))
;; NIL
;; CL-USER> (graph::find-matching-rest 0 '(4 5 7 1 2 3) graph::*forests*)
;; ((0 1 2 3 4 5) (4) (4) (NIL NIL NIL) (NIL) (NIL))
;; CL-USER> (graph::a2b   4 5  )
;; ((5 (4 5 7 1 2 3)) (7 (2)) (8 (9 10 6)) (9 (8)) (10 (8))) NIL
;; (5 7 8 9 10)
;; (T NIL NIL NIL NIL)
;; ((0 1 2 3 4 5) (4) (NIL NIL NIL) (NIL) (NIL))
;; NIL
;; CL-USER> (graph::find-matching-rest 0 '(4 5 7 1 2 3) graph::*forests*)
;; ((0 1 2 3 4 5) (4) (NIL NIL NIL) (NIL) (NIL))
;; CL-USER> (graph::a2b   5 7  )
;; ((7 (4 5 7 1 2 3)) (8 (9 10 6)) (9 (8)) (10 (8))) NIL
;; (7 8 9 10)
;; (T NIL NIL NIL)
;; ((0 1 2 3 4 5) (NIL NIL NIL) (NIL) (NIL))
;; NIL
;; CL-USER> (graph::find-matching-rest 0 '(4 5 7 1 2 3) graph::*forests*)
;; ((0 1 2 3 4 5) (NIL NIL NIL) (NIL) (NIL))
;; CL-USER> (graph::find-matching-rest 1 '(9 10 6) graph::*forests*)
;; ((0 1 2) (NIL) (NIL))
;; CL-USER> (graph::a2b   8 9  )
;; ((7 (4 5 7 1 2 3)) (9 (8 9 10 6)) (10 (8))) NIL
;; (7 9 10)
;; (T NIL NIL)
;; ((0 1 2 3 4 5) (NIL NIL NIL NIL) (NIL))
;; NIL
;; CL-USER> (graph::find-matching-rest 1 '(8 9 10 6) graph::*forests*)
;; ((0 1 2 3) (0))
;; CL-USER> (graph::a2b    9 10  )
;; ((7 (4 5 7 1 2 3)) (10 (8 9 10 6))) NIL
;; (7 10)
;; (T NIL)
;; ((0 1 2 3 4 5) (NIL NIL NIL NIL))
;; NIL
;; CL-USER> (graph::find-matching-rest 1 '(8 9 10 6) graph::*forests*)
;; ((0 1 2 3))
;; CL-USER> (graph::find-matching-rest 0 '(8 9 10 6) graph::*forests*)
;; ((NIL NIL NIL NIL NIL NIL) (0 1 2 3))
;; CL-USER>
