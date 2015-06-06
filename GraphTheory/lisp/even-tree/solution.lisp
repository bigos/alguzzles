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
    (loop for cc in (comb n edges)
       with z = edges
       with results
       with resdone
       until done
       do
         (progn
           (setq z edges)
           (loop for c in cc
              do (setq z (remove-if (lambda (x) (equalp x c)) z)))
           (format T "~&~A | ~a   " z cc)
           (initialize z)
           (doit)
           (setq results
                 (map 'list #'evenp (loop for f in *forests*
                                       collect (length (cadr f)))))
           (setq resdone (notany #'null results))
           (when resdone
             (format t "~&~A -----  ~A ~A~%"
                     *forests*
                     results
                     resdone)
             (format t "~&~afinish me" cc)
             (format t "  ~D <<<<<~%"(length cc)))))))

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
  (map 'list (lambda (x) (if (or (equalp (car x) seek)
                                 (not (not (position seek (cadr x)))))
                             (car x)
                             nil))
       connections))

(defun find-matching-rest (seeks connections) ;stuck again
  (map 'list
       (lambda (z) (if (common-els? (cadr z) seeks)
                       (car z)
                       nil))
       connections))

(defun common-els? (l1 l2)
  (some  (lambda (x) (if x T nil))
         (map 'list
              (lambda (z) (not (null z)))
              (map 'list (lambda (x) (position x l2)) l1))))

(defun my-matches (pos)
  (list
   (remove-if #'null
              (find-matching-first (car (elt *forests* pos)) *forests*))
   (remove-if #'null
              (find-matching-rest  (cadr (elt *forests* pos)) *forests*))))

(defun all-matches ()
  (loop for x from 0 below (length (connection-points *forests*))
     collect (my-matches x)))

(defun all-matches-print ()
  (loop for x  in (all-matches)
     do (format t "~a~%~%" x)))

(defun zap (pos)
  (when pos
    (let ((m (my-matches pos)))
      (cond ((>= (length (car m)) 2) (apply #'a2b (subseq (car m) 0 2)) T)
            ((>= (length (cadr m)) 2) (apply #'a2b (subseq (cadr m) 0 2)) T)
            (T nil)))))

(defun finishedp ()
  (map 'list
       (lambda (x) (and (eq 1 (length (car x)))
                        (eq 1 (length (cadr x)))))
       (all-matches)))

(defun all-finishedp ()
  (every (lambda (x) (not (null x)))
         (finishedp)))

(defun first-null (nlist)
  (let ((ll (length nlist)))
      (loop for x from 0 below ll
         until (null (elt nlist x))
         finally (return (if (< x ll) x nil)))))

(defun doit ()
  (loop
     until (null (zap (first-null (finishedp))))))

(defun a2b (a b)
  (let ((old-cpts (connection-points *forests*))
        (cpts))
    (if (and (position a old-cpts)
             (position b old-cpts))
        (progn
          (setq *forests* (move-connections a b *forests*))
          (setq cpts (connection-points *forests*))
          (format nil "~&~A - ~A : ~A ~&~A~&~a~&~A~%"
                  a b
                  *forests*
                  cpts
                  (find-matching-first (car (elt *forests* 0)) *forests*)
                  (find-matching-rest  (cadr (elt *forests* 0)) *forests*)))
        (progn
          (format t "~&error~%")))))

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
    (format t "ar is: ~A~%" ar)
    (initialize ar)
    (doit)
    (format T "~D~%"
            (length (finishedp)))))

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
                                    "input1.txt"))
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
