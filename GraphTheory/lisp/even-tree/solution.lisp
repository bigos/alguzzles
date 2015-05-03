;; (require :sb-sprof)

(declaim (optimize (speed 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :graph
  (:use :common-lisp))

(in-package :graph)

(defparameter *original-edges* nil)

(defun initialize (edges)
  (format t "~%~A ~%" edges)
  (setq *original-edges* edges))

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

(defparameter *forests* nil)

(defun forests (edges)
  (let ((connections (connections edges))
        (forestlings (loop for n in (graph::nodes edges) collect (list n))))
    ;; removing forestlings
    ;; (delete-if (lambda (x) (equalp x '(1)))
    ;;            (loop for n in (graph::nodes (arx)) collect (list n)))

    ;; findin which forest has the forestling
    ;; (loop for n in '((10 9) (8) (6) (7) (5) (4 3) (2) (1))
    ;;    until (position 9 n) finally (return n))
    forestlings))

;;;;;;;;;;;;;;;;; old neighbours code ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun find-forest-inner (node nodes)
;;   (loop for n in (neighbours node nodes) do
;;        (unless (position n *found*)     ; sloooooow!
;;          (pushnew n *found*)
;;          (find-forest-inner n nodes))))

;; (defun find-forest (node1 nodes1)
;;   (setq *found* nil)
;;   (find-forest-inner node1 nodes1)
;;   *found*)

;; (defun in-forests? (node forests)
;;   (let ((result))
;;     (loop for f in forests do
;;          (when (position node f)
;;            (setq result T)))
;;     result))

;; (defun all-even? (forests)
;;   (let ((res T))
;;     (loop for f in forests do
;;          (when (oddp (length f))
;;            (setq res nil)))
;;     res))

;; (defun forests (nodes)
;;   (let ((fs))
;;     (loop for v in (vertices nodes) do
;;          (unless (in-forests? v fs)
;;            (push (find-forest v nodes) fs)))
;;     fs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;; finally return to repl package ;;;;

(in-package :common-lisp-user)

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
