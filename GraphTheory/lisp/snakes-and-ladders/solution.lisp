;;; notes

;;; not reachable if sequence of snake mouths larger than max step and no
;;; ladders over it originating from reachable code in previous nodes

;; instead of trying ~ 100 ^ 6 combinations trying every dice possibility
;; why don't I figure out a graph merging series of small dice throws int one longer
;; find the shortest path and then (truncate long 6)

(defun node-type (n ladders snakes)
  (cond
    ((find-if (lambda (x) (eq n (car x))) ladders) (list 'lb))
    ((find-if (lambda (x) (eq n (cadr x))) ladders) (list 'lt))
    ((find-if (lambda (x) (eq n (car x))) snakes) (list 'sm))
    ((find-if (lambda (x) (eq n (cadr x))) snakes) (list 'st))
    (T 'hmm)))

(defun node-types (ladders snakes)
  (loop for n in (sorted-list-of-nodes ladders snakes)
     collect (cons n (node-type n ladders snakes))))

(defun sorted-list-of-nodes (ladders snakes)
  (concatenate 'list
               '(1)
               (sort (loop for n in (concatenate 'list ladders snakes)
                        collect (car n) collect (cadr n)) '<)
               '(100)))

(defun sorted-special-nodes (ladders snakes)
  (let ((sn (sort (loop for n in (concatenate 'list ladders snakes)
                     collect n)
                  (lambda (x y) (< (car x) (car y))))))
    (concatenate 'list
                 (list (list 1 (caar sn)))
                 sn
                 (list (list (caar (last sn)) 100)))))

(defparameter *visited* nil)

(defun visited-p (n)
  (find n *visited*))

(defun add-to-visited (n)
  (push n *visited*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (subseq (arr-connections (ladders-1) (snakes-1) 100 6)
;;         95)
;;
(defun arr-connections (ladders snakes &optional (max-val 100) (max-step 6))
  (let* ((my-arr (make-array max-val :initial-element nil)))
    (loop for x from 1 below max-val do
         (setf (elt my-arr x)
               (loop for d from 1 to max-step
                  when (<= (+ x d) max-val)
                  collect (+ x d))))
    (loop for l in ladders do
         (setf (elt my-arr (car l))
               (cdr l)))
    (loop for l in snakes do
         (setf (elt my-arr (car l))
               (cdr l)))
    my-arr))


;; obsolete
;; ;; (solve-me (ladders-1) (snakes-1))
(defun solve-me (ladders snakes)
  ;; (let ((edges (append (boustrophedon)
  ;; ladders
  ;; snakes)))
  ;; (format t "~&edges ~A~%" edges)
  ;; (format t "connections ~A~%" (connections edges)))
  (format t "~A~A~%" ladders snakes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ladders-1 ()
  '((8 52) (6 80) (26 42) (2 72)))

(defun snakes-1 ()
  '((51 19) (39 11) (37 29) (81 3) (59 5) (79 23) (53 7) (43 33) (77 21)))

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
  (let* ((test-cases (parse-integer (read-line stream)))
         (ladders) (ll)
         (snakes) (ss))
    (dotimes (tc test-cases )
      (setq ladders (parse-integer (read-line stream)))
      (setq ll (loop for l from 0 below ladders
                  collecting (split-and-parse (read-line stream))))
      (setq snakes (parse-integer (read-line stream)))
      (setq ss (loop for s from 0 below snakes
                  collecting (split-and-parse (read-line stream))))
      (format t "ladders ~A~%snakes ~A~%" ll ss)
      (solve-me ll ss))))

 ;; (solution) ; uncomment this when running on hacker-rank

;;; still need to add  removing vertices
(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "GraphTheory/lisp/snakes-and-ladders/"
                                    "input0.txt"))
      (solution s))))
