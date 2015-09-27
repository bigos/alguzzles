;;; notes

;;; not reachable if sequence of snake mouths larger than max step and no
;;; ladders over it originating from reachable code in previous nodes

;; instead of trying ~ 100 ^ 6 combinations trying every dice possibility
;; why don't I figure out a graph merging series of small dice throws int one longer
;; find the shortest path and then (truncate long 6)


;;; need to think about snakes
(defun arr-markovs-from (n special-nodes &optional (max-step 6))
  (let ((special (find-if (lambda (x) (eq n (caar x)))
                          special-nodes ))
        (kind))
    (if special
        (progn
          (setf kind (cadr special))
          (case kind
            (lb "lb up")
            (lt "top of the ladder")
            (sm "progressssss eater")
            (st "ogon")))
        (loop for f from max-step downto 1
           when (>= (- n f) 1)
           collect (- n f))
        )))

;;; need to think about ladders
(defun arr-markovs-to (n special-nodes &optional (max-val 100) (max-step 6))
  (loop for f from 1 to max-step
     when (<= (+ n f) max-val)
     collect (+ n f)))

;;; not finished yet
;;; still have to take into consideration snakes and ladders
(defun arr-markovs (ladders snakes &optional (min-val 1) (max-val 100))
  (let ((my-arr (make-array (+ 1 max-val) :initial-element nil))
        (special-nodes (sorted-special-nodes ladders snakes)))
    ;; new attempt will go  here
    (loop for n from min-val to max-val do
         (setf (elt my-arr n)
               (list n
                     (arr-markovs-from n special-nodes)
                     (arr-markovs-to n special-nodes))))
    my-arr))

(defun ladder-nodes (nodes)
  (loop for n in nodes
     collect  (list n 'lb)
     collect (list (list (cadr n) (car n)) 'lt)))

(defun snake-nodes (nodes)
  (loop for n in nodes
     collect  (list n 'sm)
     collect (list (list (cadr n) (car n)) 'st)))

(defun sorted-special-nodes (ladders snakes)
  (sort (concatenate 'list
                     (ladder-nodes ladders)
                     (snake-nodes snakes))
        (lambda (x y) (< (caar x) (caar y)))))

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
