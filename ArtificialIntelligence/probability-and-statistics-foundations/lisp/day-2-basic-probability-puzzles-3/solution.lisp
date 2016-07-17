;;; need to understand probability

(defun probability (wanted-outcomes possible-outcomes)
  "calculate probability dividing number of wanted by number of
  possible outcomes"
  (/ wanted-outcomes
     possible-outcomes))

;;; interesting sequence


(defun seq (x every-unit units)
  (mod (floor (/ x every-unit)) units))

;;; call example (test3d 45 3 4 12)
(defun test3d (upto d1 d2 d3)
  (loop for x from 0 to upto
     collect
       (list x
             '-
             (seq x 1 d1)
             (seq x d1 d2)
             (seq x d3 upto))))
