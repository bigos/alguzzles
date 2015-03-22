(require :sb-sprof)

;;; (declaim (optimize speed))

(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))



(defun split-to-forests (gr)
  (let* ((removal-combinations (subsequences gr))
         (new-forest)
         (verts)
         (forests)
         (res))
    (loop for e in removal-combinations do
         (setq new-forest (delete-pairs gr e))
         (setq verts (vertices gr))
         (setq forests (forests new-forest))
         (if (and (all-even? forests)
                  (eq (length verts)
                      (apply #'+  (map 'list #'length forests))) )
             (progn
               (setq res e)
               (format nil "------ ~A ~a ~%~%" e new-forest)))
       until res)
    (length res)))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
              append (mapcar (lambda (l) (cons element l))
                             (all-permutations (remove element list)))))))

;; usage
;; (comb 3 '(0 1 2 3 4 5) #'print)
(defun comb (m list fn)
  (labels ((comb1 (l c m)
             (when (>= (length l) m)
               (if (zerop m) (return-from comb1 (funcall fn c)))
               (comb1 (cdr l) c m)
               (comb1 (cdr l) (cons (first l) c) (1- m)))))
    (comb1 list nil m)))

(defun subsequences (list)
  (let ((res))
    (loop for l from 1 to (1- (length list))
       collecting (comb l list (lambda (x) (push x res))))
    res))

(defun eq-pair (pair1 pair2)
  (or (equal pair1 pair2)
      (equal pair1 (cons (cdr pair2) (car pair2)))))

(defun delete-pairs (lst pairs)
  (loop for pair in pairs do
       (setq lst (remove-if (lambda (x) (eq-pair x pair)) lst))
       )
  lst)

(defun remove-pair (lst pair)
  (remove-if (lambda (x) (eq-pair x pair)) lst))

;; (neighbour-list 8 (dynamic-split-arr))
(defun neighbour-list (node)
  (neighbours node (dynamic-split-arr)))

;;; insert your code here
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
                       (unless (position n found)
                         (progn
                           (pushnew n found)
                           (forest n nodes)))))))
      (forest node1 nodes1))
    found))

(defun node-in-forest? (node forest)
  (position node forest))

(defun in-forests? (node forests)
  (let ((result))
    (loop for f in forests do
         (when (node-in-forest? node f)
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

(defun splitters () '((3 . 1) (6 . 1)))
(defun split-arr () '((10 . 8) (9 . 8) (8 . 6) (7 . 2) (5 . 2) (4 . 3) (2 . 1)))
(defun dynamic-split-arr () (delete-pairs (arr) (splitters)))

(defun solution (&optional stream)
  (let* ((dd (split-and-parse (read-line stream)))
         (n (car dd))
         (m (cadr dd))
         (ar))
    (loop for x in
         (loop for row below m
            collecting (split-and-parse (read-line stream)))
       do (push (cons (car x) (cadr x)) ar))

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
(sb-sprof:with-profiling (:max-samples 1000 :report :flat :loop nil))
(sb-sprof:start-profiling)
(repl-main)
(sb-sprof:report)


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
