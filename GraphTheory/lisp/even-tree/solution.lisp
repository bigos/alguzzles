;; (require :sb-sprof)

;; (declaim (optimize (speed 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun comb (m list)
  (let ((res))
    (labels ((comb1 (l c m)
               (when (>= (length l) m)
                 (if (zerop m) (return-from comb1 (push c res)))
                 (comb1 (cdr l) c m)
                 (comb1 (cdr l) (cons (first l) c) (1- m)))))
      (comb1 list nil m))
    res))

(defun remove-pairs (n edges)
  (let ((done))
    (loop for cc in (comb n (without-leaves edges))
       with z
       with forrests
       do (progn
            (setq z edges)
            (loop for c in cc
               do (setq z (remove-if (lambda (x) (equalp x c)) z))
               finally (setq forrests (forrests z)))
            (setq done (notany #'null
                               (map 'list
                                    (lambda (x) (evenp (length x)))
                                    forrests))))
       until done
       finally (return (if done
                           (list forrests "-" cc)
                           nil)))))

(defparameter *original-edges* nil)
(defparameter *forests* nil)
(defparameter *nodes* nil)

(defun initialize (edges)
  (format nil "~%~A ~%" edges)
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

;;; need to skip edges that include leaves
(defun leaves (edges)
  (remove-if #'null
             (map 'list
                  (lambda (x) (if (eq 1 (length (cadr x)))
                                  (car x)
                                  nil))
                  edges)))

(defun without-leaves (edges)
  (let ((leaves (leaves (connections edges))))
    (remove-if (lambda (x)  (or (position (car x) leaves)
                                (position (cdr x) leaves)))
               edges)))

(defun connections-for (node connections)
  (loop for c in connections
     until (eq node (car c))
     finally (return (cadr c))))

(defun forrest-for (node edges)
  (let ((connections (connections edges))
        (visited))
    (labels ((find-forest (n)
               (setq visited (nconc visited (list n)))
               (loop for c in (connections-for n connections) do
                    (unless (position c visited) (find-forest c)))))
      (find-forest node)
      visited)))

(defun forrests (edges)
  (let* ((nodes (nodes edges))
         (current-forest)
         (found-forests))
    (loop do (progn
               (setq current-forest (forrest-for (car nodes) edges))
               (push current-forest found-forests)
               (loop for a in current-forest
                  do
                    (setq nodes (delete-if (lambda (x) (eq a x)) nodes))))
       until (null nodes)
         finally (return found-forests))))

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

(defun arr1 ()
  '((20 . 8) (19 . 1) (18 . 10) (17 . 6) (16 . 6) (15 . 12) (14 . 8)
    (13 . 7) (12 . 3) (11 . 10) (10 . 7) (9 . 2) (8 . 1) (7 . 1) (6 . 5)
    (5 . 2) (4 . 3) (3 . 1) (2 . 1)))

(defun arr2 ()
  '((30 . 25) (29 . 4) (28 . 27) (27 . 5) (26 . 17) (25 . 21)
    (24 . 12) (23 . 2) (22 . 20) (21 . 15) (20 . 4) (19 . 18)
    (18 . 17) (17 . 1) (16 . 10) (15 . 8) (14 . 2) (13 . 2) (12 . 8)
    (11 . 4) (10 . 4) (9 . 5) (8 . 1) (7 . 4) (6 . 4) (5 . 2) (4 . 3)
    (3 . 2) (2 . 1)))

(defun arr6 ()
  '((70 . 10) (69 . 56) (68 . 39) (67 . 62) (66 . 12) (65 . 20) (64 . 45)
    (63 . 10) (62 . 35) (61 . 1) (60 . 22) (59 . 39) (58 . 43) (57 . 3)
    (56 . 9) (55 . 37) (54 . 41) (53 . 44) (52 . 23) (51 . 4) (50 . 28)
    (49 . 38) (48 . 20) (47 . 24) (46 . 31) (45 . 43) (44 . 40) (43 . 7)
    (42 . 32) (41 . 21) (40 . 26) (39 . 35) (38 . 21) (37 . 35) (36 . 27)
    (35 . 15) (34 . 25) (33 . 30) (32 . 7) (31 . 29) (30 . 4) (29 . 23)
    (28 . 27) (27 . 4) (26 . 7) (25 . 6) (24 . 1) (23 . 10) (22 . 16)
    (21 . 11) (20 . 10) (19 . 3) (18 . 5) (17 . 15) (16 . 9) (15 . 14)
    (14 . 13) (13 . 2) (12 . 5) (11 . 4) (10 . 5) (9 . 6) (8 . 7) (7 . 1)
    (6 . 2) (5 . 3) (4 . 2) (3 . 2) (2 . 1)))

(defun arr7 ()
  '((80 . 54) (79 . 12) (78 . 29) (77 . 25) (76 . 47) (75 . 17) (74 . 66)
    (73 . 12) (72 . 28) (71 . 36) (70 . 50) (69 . 14) (68 . 28) (67 . 26)
    (66 . 56) (65 . 5) (64 . 33) (63 . 22) (62 . 35) (61 . 43) (60 . 40)
    (59 . 26) (58 . 52) (57 . 37) (56 . 33) (55 . 45) (54 . 46) (53 . 6)
    (52 . 33) (51 . 9) (50 . 47) (49 . 46) (48 . 1) (47 . 5) (46 . 38)
    (45 . 10) (44 . 9) (43 . 11) (42 . 22) (41 . 16) (40 . 18) (39 . 29)
    (38 . 14) (37 . 11) (36 . 15) (35 . 16) (34 . 18) (33 . 23) (32 . 14)
    (31 . 13) (30 . 23) (29 . 28) (28 . 10) (27 . 22) (26 . 4) (25 . 10)
    (24 . 16) (23 . 1) (22 . 9) (21 . 9) (20 . 9) (19 . 3) (18 . 12)
    (17 . 10) (16 . 12) (15 . 10) (14 . 5) (13 . 6) (12 . 9) (11 . 8)
    (10 . 2) (9 . 8) (8 . 7) (7 . 6) (6 . 3) (5 . 3) (4 . 2) (3 . 1)
    (2 . 1)))

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
    (loop for x
       from (length ar)
       downto 1
       until (remove-pairs x ar)
       finally (princ x))
    ))

 ;; (solution) ; uncomment this when running on hacker-rank

;;; still need to add  removing vertices
(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "GraphTheory/lisp/even-tree/"
                                    "input6.txt"))
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
