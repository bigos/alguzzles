;; (defun remove-inner (n beginning ending)
;;   (format t "~A ~A ~A ~A~%" beginning ending (car ending) ( append beginning (list (car ending))))
;;   (block nil
;;     (if (eq n (car ending))
;;         (return (append beginning (cdr ending)))
;;         (remove-inner n
;;                       (if beginning
;;                           (append beginning (list (car ending)))
;;                           (list (car ending)))
;;                       (cdr ending)))))

;; (defun remove-first (n ens)
;;   (remove-inner n '() ens))





(defun node-type (node)
  (if (> (length (cadr node)) 1)
      (if (> (caadr node)
             (cadadr node))
          'snake
          'ladder)
      'normal))

(defun termination (moves)
  (> moves 3))

(defun move-piece (edges start end moves)
  (format T
          "~&arguments - start: ~a, end: ~a, moves: ~a    ~a~%"
          start end moves (- end start))
  (if (termination moves)
      (princ "done")
      (loop for dice from 1 to 6
         do (move-piece edges end (+ dice end) (1+ moves)))))

(defun find-shortest (edges)
  (loop for dice from 1 to 6
     do
       (move-piece edges 1 dice 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun children-of (n edges)
  (loop for e in edges when (eq  (car e) n) collect (cadr e)))

(defun connections (edges)
  (loop for x from 1 to 100
     collecting (list x (children-of x edges))))

(defun boustrophedon ()
  (loop for x from 1 below 100
       collecting (list x (1+ x))))

(defun solve-me (ladders snakes)
  (let ((edges (append (boustrophedon)
                       ladders
                       snakes)))
    (format t "~&edges ~A~%" edges)
    (format t "connections ~A~%" (connections edges))
    (find-shortest edges)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun the-data ()
  '((1 (2)) (2 (3 72)) (3 (4)) (4 (5)) (5 (6)) (6 (7 80)) (7 (8))
    (8 (9 52)) (9 (10)) (10 (11)) (11 (12)) (12 (13)) (13 (14))
    (14 (15)) (15 (16)) (16 (17)) (17 (18)) (18 (19)) (19 (20))
    (20 (21)) (21 (22)) (22 (23)) (23 (24)) (24 (25)) (25 (26))
    (26 (27 42)) (27 (28)) (28 (29)) (29 (30)) (30 (31)) (31 (32))
    (32 (33)) (33 (34)) (34 (35)) (35 (36)) (36 (37)) (37 (38 29))
    (38 (39)) (39 (40 11)) (40 (41)) (41 (42)) (42 (43)) (43 (44 33))
    (44 (45)) (45 (46)) (46 (47)) (47 (48)) (48 (49)) (49 (50))
    (50 (51)) (51 (52 19)) (52 (53)) (53 (54 7)) (54 (55)) (55 (56))
    (56 (57)) (57 (58)) (58 (59)) (59 (60 5)) (60 (61)) (61 (62))
    (62 (63)) (63 (64)) (64 (65)) (65 (66)) (66 (67)) (67 (68))
    (68 (69)) (69 (70)) (70 (71)) (71 (72)) (72 (73)) (73 (74))
    (74 (75)) (75 (76)) (76 (77)) (77 (78 21)) (78 (79)) (79 (80 23))
    (80 (81)) (81 (82 3)) (82 (83)) (83 (84)) (84 (85)) (85 (86))
    (86 (87)) (87 (88)) (88 (89)) (89 (90)) (90 (91)) (91 (92))
    (92 (93)) (93 (94)) (94 (95)) (95 (96)) (96 (97)) (97 (98))
    (98 (99)) (99 (100)) (100 NIL)))

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
