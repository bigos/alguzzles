;;; notes

;;; not reachable if sequence of snake mouths larger than max step and no
;;; ladders over it originating from reachable code in previous nodes

;; instead of trying ~ 100 ^ 6 combinations trying every dice possibility
;; why don't I figure out a graph merging series of small dice throws int one longer
;; find the shortest path and then (truncate long 6)

;; 1..9 possible steps 1, 2
;; (arr-connections '((4 7)) '((6 3)) 10 2)
;;       -------  4 -> 7
;;       |     |
;; 1-2-3-4 5-6 7-8-9
;;     |     |
;;     -------  6 -> 3

;; (1 2) (1 3) -> (1 4)
;; (2 3) (2 4)
;; (3 4) (3 5) -> (3 5) is within reach
;; (4 7)       -> (4 7)
;; (5 6) (5 7)
;; (6 3)
;; (7 8) (7 9 =) -> (7 9 =)
;; (8 9 =)

;; a..p steps 1,2
;;           ------------------- f->o, g->o
;;           | |               |
;;     ------+-+--  c->h, d->h |
;;     | |   | | |             |
;; a-b-c-d e-f-g h-i-j-k-l m-n-o-p =
;;         |           | |
;;         --------------- k->e, l->e

;; StartEndLength
;; ab1 ac2
;; bc1 bd2
;; ch0
;; dh0
;; ef1 eg2
;; fo0
;; go0
;; hi1 hj2
;; ij1 ik2
;; jk1 jl2
;; ke0
;; le0
;; mn1 mo2
;; no1 np2=
;; op1= =

;; example path of 9 steps / 5 dice throws
;; ac2 ch0 hj2 jl2 le0 eg2 go0 op1=


;; loop steps 1,2
;;           ------- f->i, g->i
;;           | |   |
;;     ------+-+-- | c->h, d->h
;;     | |   | | | |
;; a-b-c-d e-f-g h-i-j-k-l m-n-o-p =  LOOP! m..p never reached
;;         |           | |
;;         --------------- k->e, l->e

;; (arr-connections '((3 8) (4 8) (6 9) (7 9)) '((11 5) (12 5)) 16 2)
;; ab1 ac2
;; bc1 bd2
;; ch0
;; dh0
;; ef1 eg2
;; fi0
;; gi0
;; hi1 hj2
;; ij1 ik2 LOOP
;; jk1 jl2 LOOP
;; ke0
;; le0
;; mn1 mo2  NEVER REACHED
;; no1 np2= NEVER REACHED
;; op1= =   NEVER REACHED

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
