;;; pseudocode
;; given I am at first step S
;; I can go to S+1 to S+6 or top of a ladder if S is bottom of a ladder
;; provided S is not mouth of a snake
;;

;;; pseudoworking
(defun pseudo (n ladders snakes)
  (let ((l) (s))
    (format t "~&n=~A  " n)
    (if (visited-p n)
        (format t " visited> ~A ~%" n)
        (progn
          (add-to-visited n)
          (format t " !~A! " n)
          (if (>= n 100)
              (format t "~&done ~A~%" n)
              (progn
                (setq l (car (loop for x in ladders when (eq (car x) n) collect x)))
                (setq s (car (loop for x in snakes  when (eq (car x) n) collect x)))
                (if l
                    (progn
                      (format t "~&jumping ladder from ~A to ~A debug ~A ~A~%" n (cadr l) l s)
                      (pseudo (cadr l) ladders snakes))
                    (if s
                        (progn
                          (format t "~&boo! descending from ~a to ~A~%" n (cadr s))
                          (pseudo (cadr s) ladders snakes))
                        (loop for x from 6 downto 1 do
                             (progn (format t " trying ~A to ~A " n (+ n x))
                                    (pseudo (+ n x) ladders snakes)))))))))))


(defparameter *visited* nil)

(defun visited-p (n)
  (find n *visited*))

(defun add-to-visited (n)
  (push n *visited*))

(defun node-type (node)
  (when (> (length (cadr node)) 1)
      (if (> (caadr node)
             (cadadr node))
          'snake
          'ladder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun connections (edges)
  (loop for x from 1 to 100
     collecting
       (list x
             (loop for e in edges
                when (eq (car e) x)
                collect (cadr e)))))

(defun boustrophedon ()
  (loop for x from 1 below 100
       collecting (list x (1+ x))))

;; (solve-me (ladders-1) (snakes-1))
(defun solve-me (ladders snakes)
  (let ((edges (append (boustrophedon)
                       ladders
                       snakes)))
    (format t "~&edges ~A~%" edges)
    (format t "connections ~A~%" (connections edges))))

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
