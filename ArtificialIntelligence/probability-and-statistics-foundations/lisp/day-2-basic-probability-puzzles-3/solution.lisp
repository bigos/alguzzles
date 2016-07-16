;;; need to understand probability

(defun probability (wanted-outcomes possible-outcomes)
  "calculate probability dividing number of wanted by number of
  possible outcomes"
  (/ wanted-outcomes
     possible-outcomes))

;;; interesting sequence
(loop for x from 1 to 12
   collect (list x
                 (mod x (/ 12 4))
                 (mod x (/ 12 3))))
