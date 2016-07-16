;;; not applicable

(defun my-random ()
  (let ((from 1)
         (upto 6)
         (max-tries (- 240 6)))
    (loop for x from 1 to max-tries
       collect (random-range from upto))))

(defun random-range (from to)
  (let ((diff (- to from)))
    (+ from (random (1+ diff)))))

(defun possibilities (from to throws)
  "Dice from 1 to 6 with 2 throws"
  (expt (- (+ 1 to) from) throws))

;;; table of sums leading to solution

;; | x | 1 | 2 | 3 | 4 | 5 | 6 |
;; |---+---+---+---+---+---+---|
;; | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
;; | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
;; | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
;; | 4 | 5 | 6 | 7 | 8 | 9 | a |
;; | 5 | 6 | 7 | 8 | 9 | a | b |
;; | 6 | 7 | 8 | 9 | a | b | c |
