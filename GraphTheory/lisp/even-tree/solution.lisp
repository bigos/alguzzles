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
       with z = edges
       with results
       with resdone
       with rescnt = 0
       until done
       do
         (progn
           (setq z edges)
           (loop for c in cc
              do (setq z (remove-if (lambda (x) (equalp x c)) z)))
           (initialize z)
           (doit)
           (setq results
                 (map 'list #'evenp (loop for f in *forests*
                                       collect (length (cadr f)))))
           (setq resdone (notany #'null results))
           (when (and resdone
                      (> (length *forests*) 1))
             (format t "~& removing ~a~%" cc)
             (setq rescnt (max rescnt (length results)))
             (format T "~&got forests~A -- ~A ---  ~A ~A~%"
                     *forests*
                     (length *forests*)
                     results
                     resdone)
             (format nil "~a   finish me~%~%" rescnt)
             ))
       until (and (> (length *forests*) 1)
                  resdone)
       finally (return resdone) )))

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

(defun forrest-for (node connections)
  (let ((res) (len-res))
    (labels ((doit (n)
               (loop for nx in (connections-for n connections)
                  do (progn (setq res
                                  (concatenate 'list
                                               (connections-for nx connections)
                                               res))
                            (setq res (concatenate 'list
                                                   (list  nx)
                                                   res))))
               (setq res (remove-duplicates res))))
      (setq res (push node res))
      (setq res (concatenate 'list (connections-for node connections) res))
      (setq res (remove-duplicates res))
      (setq len-res (length res))
      (loop for na in res
         do
           (doit na)
         until (eq len-res
                   (length res))))
    res))

(defun forrests (edges)
  (let* ((connections (connections edges))
         (nodes (nodes edges))
         (current-forest)
         (nodes-without-the-forest)
         (found-forests))
    (loop do (progn
               (format t "nodes before ~A --~%" nodes)
               (setq current-forest (forrest-for (car nodes) connections))
               (push current-forest found-forests)
               (loop for a in current-forest do
                    (setq nodes-without-the-forest (remove-if (lambda (x) (eq a x)) nodes)))
               (setq nodes nodes-without-the-forest)
               (format t "nodes after ~A~%" nodes)
               (format t "forests ~A~%" found-forests))
         until (null nodes))))

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
    (format nil "ar is: ~A~%" ar)
    (loop for x
       from (length (without-leaves ar))
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
                                    "input3.txt"))
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
