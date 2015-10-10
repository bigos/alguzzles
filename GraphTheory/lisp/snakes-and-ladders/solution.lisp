;;; notes

;;; not reachable if sequence of snake mouths larger than max step and no
;;; ladders over it originating from reachable code in previous nodes

;; instead of trying ~ 100 ^ 6 combinations trying every dice possibility
;; why don't I figure out a graph merging series of small dice throws int one longer
;; find the shortest path and then (truncate long 6)

(defun prev-pos (n)
  (loop for f from 6 downto 1
     when (>= (- n f) 1)
     collect (- n f)))

(defun next-pos (n &optional (max-val 100))
  (loop for f from 1 to 6
     when (<= (+ n f) max-val)
     collect (+ n f)))

(defun special-node (n special-nodes)
  (find-if (lambda (x) (eq n (caar x)) ) special-nodes ))

(defun special-node-type (n special-nodes)
  (cadr (special-node n special-nodes)))

;; can't arrive from sm or lb
(defun arr-markovs-from (n special-nodes)
  (let ((special (special-node n special-nodes)))
    (remove-if 'null
               (map 'list
                    (lambda (x)
                      (if (or (eq 'lb (special-node-type x special-nodes))
                              (eq 'sm (special-node-type x special-nodes)))
                          nil
                          x))
                    (concatenate 'list
                                 (when (eq 'lt (cadr special))
                                   (prev-pos (cadar special))) ;except sm and lb
                                 (when (eq 'st (cadr special))
                                   (prev-pos (cadar special))) ;except sm and lb
                                 (prev-pos n)))))) ;except sm and lb

(defun arr-markovs-to (n special-nodes)
  ;; do not include snake mouths, but raplace them with tails
  ;; do not include ladder bottoms but replace them with tops
  ;; probably need to rewrite it using some kind of recursion
  (let ((special (special-node n special-nodes)))
    (if special
        (cond ((eq 'lb (cadr special)) (next-pos (cadar special)))
              ((eq 'sm (cadr special)) (next-pos (cadar special)))
              (t (next-pos n)))
        (next-pos n))))

;;; not finished yet
;;; still have to take into consideration snakes and ladders

;;; got through list of snake mouths and ladder bottoms
;;; and remove them from incoming positions (caddr) of lists in my-arr
(defun arr-markovs (ladders snakes &optional (min-val 1) (max-val 100))
  (let ((my-arr (make-array (+ 1 max-val) :initial-element nil))
        (special-nodes (sorted-special-nodes ladders snakes)))
    (loop for n from min-val to max-val do
         (setf (elt my-arr n)
               (list n
                     (cadr (special-node n special-nodes))
                     (arr-markovs-from n special-nodes)
                     (arr-markovs-to n special-nodes))))
    ;; new attempt will go  here
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

;;;:::::::::::::::::::::::::::::::::::::::::::::::::::
(defparameter *nodes* (arr-markovs (ladders-1) (snakes-1)))
(defparameter *found* nil)
(defparameter *visited* nil)
(defparameter *tosses* nil)

(defun visited-p (n)
  (find n *visited*))

(defun add-to-visited (n)
  (push n *visited*))

(defun neighbours (n) (caddr (aref *nodes* n)))

(defun breadth-first (start end tosses)
  (let ((neighbours (neighbours start)))
    (add-to-visited start)
     (format t "~&examining ~A tosses ~A~%" start tosses)
    (if (find end neighbours)
        (progn
          (push tosses *tosses*)
          ; (format t "~&found in ~a tosses" tosses)
          (setf *found* T))
        (loop for n in neighbours do
             (unless (visited-p n)
               (breadth-first n end (1+ tosses)))))))

(defun shortest-path () (car (sort *tosses* '<)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; obsolete
;; ;; (solve-me (ladders-1) (snakes-1))
(defun solve-me (ladders snakes)
  (setf *found* nil *visited* nil *tosses* nil)
  (defparameter *nodes* (arr-markovs ladders snakes))
  (breadth-first 100 1 1)
  (format t "~&tosses ~A~%" *tosses*)
  (format t "~A~%" (shortest-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ladders-0 () '((32 62) (42 68) (12 98)))
(defun snakes-0 () '((95 13) (97 25) (93 37) (79 27) (75 19) (49 47) (67 17)))

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
