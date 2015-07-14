(defun remove-first (n numbers &optional nx )

)

(defun move-piece (edges start end moves)
  (if (termination)
      (princ done)
      (loop for dice from 1 to 6
         do (move-piece edges end (+ dice end) (1+ moves)))))

(defun find-shortest (edges)
  (loop for dice from 1 to 6
     do (progn
          (move-piece (edges 1 d 0)))))

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
