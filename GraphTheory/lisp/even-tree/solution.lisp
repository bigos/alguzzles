(require :sb-sprof)

(declaim (optimize (speed 3)))

(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun split-inner (gr e)
  ;; (format nil "~A~%" e)
  (let* ((new-forest (delete-pairs gr e))
         (forests (forests new-forest)))
    (when (and (all-even? forests)
               (eq (length (vertices gr))
                   (apply #'+  (map 'list #'length forests))))
        ;; (format T "------ ~A ~a ~%~%" e new-forest)
        (setq *last-found-length* (length  e)))))

;; this is the way to go
(defun split-to-forests (gr)
  (subsequences gr (lambda (x) (split-inner gr x)))
  *last-found-length*)

;; usage
;; (comb 3 '(0 1 2 3 4 5) #'print)
(defun comb (m list fn)
  (labels ((comb1 (l c m)
             (when (>= (length l) m)
               (if (zerop m) (return-from comb1 (funcall fn c)))
               (comb1 (cdr l) c m)
               (comb1 (cdr l) (cons (first l) c) (1- m)))))
    (comb1 list nil m)))

(defparameter *last-found-length* 0)

(defun subsequences (list fn)
  (loop for l from 1 below (length list)
     until (> l (+ *last-found-length* 1))
     do
       ;; (sb-ext:gc :full t)
       (comb l list fn)))

(defun eq-pair (pair1 pair2)
  (or
   (and (eq (car pair1) (car pair2))
        (eq (cdr pair1) (cdr pair2)))
   (and (eq (car pair1) (cdr pair2))
        (eq (cdr pair1) (car pair2)))))

(defun delete-pairs (lst pairs)
  (loop for pair in pairs do
       (setq lst (remove-if (lambda (x) (eq-pair x pair)) lst))
       )
  lst)

(defun connections (gr)
  (loop for v in (vertices gr)
     collect (list v (neighbours v gr))))

(defun remove-pair (lst pair)
  (remove-if (lambda (x) (eq-pair x pair)) lst))

(defun neighbours (node nodes)
  (loop for n in nodes
     when (eq node (car n)) collect (cdr n)
     when (eq node (cdr n)) collect (car n)))

(defun vertices (nodes)
  (remove-duplicates
   (loop for n in nodes
      append (list (car n) (cdr n)))))

(defparameter *found* nil)

(defun find-forest (node1 nodes1)
  (let ((found))
    (labels ((forest (node nodes)
                (let ((ns (neighbours node nodes)))
                  (loop for n in ns do
                       (unless (position n found) ; sloooooow!
                         (progn
                           (pushnew n found)
                           (forest n nodes)))))))
      (forest node1 nodes1))
    found))

(defun in-forests? (node forests)
  (let ((result))
    (loop for f in forests do
         (when (position node f)
           (setq result T)))
    result))

(defun all-even? (forests)
  (let ((res T))
    (loop for f in forests do
         (when (oddp (length f))
           (setq res nil)))
    res))

(defun forests (nodes)
  (let ((fs))
    (loop for v in (vertices nodes) do
         (unless (in-forests? v fs)
           (push (find-forest v nodes) fs)))
    fs))

(defun arr ()
  '((10 . 8) (9 . 8) (8 . 6) (7 . 2) (6 . 1) (5 . 2) (4 . 3) (3 . 1) (2 . 1)))

(defun arr2 ()
  '((30 . 25) (29 . 4) (28 . 27) (27 . 5) (26 . 17) (25 . 21)
    (24 . 12) (23 . 2) (22 . 20) (21 . 15) (20 . 4) (19 . 18)
    (18 . 17) (17 . 1) (16 . 10) (15 . 8) (14 . 2) (13 . 2) (12 . 8)
    (11 . 4) (10 . 4) (9 . 5) (8 . 1) (7 . 4) (6 . 4) (5 . 2) (4 . 3)
    (3 . 2) (2 . 1)))

;; (defun splitters () '((3 . 1) (6 . 1)))
;; (defun split-arr () '((10 . 8) (9 . 8) (8 . 6) (7 . 2) (5 . 2) (4 . 3) (2 . 1)))
;; (defun dynamic-split-arr () (delete-pairs (arr) (splitters)))

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
    (format t "~A~%" (split-to-forests ar))))

 ;; (solution) ; uncomment this when running on hacker-rank


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
