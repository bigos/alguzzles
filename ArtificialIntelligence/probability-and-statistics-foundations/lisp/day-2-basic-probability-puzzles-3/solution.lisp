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

;;; usage (varied-variations 24 3 '(4 3 2))
(defun varied-variations (n l level-limits)
  (let ((buckets
         (make-array l :initial-element 0))
        (limit
         (make-array l :initial-contents level-limits))
        (results))
    (labels ((reset-bucket (level)
               (loop for x from 0 below level do (setf (elt buckets x) 0)))
             (this-level-limit (level)
               (elt limit level))
             (variation (level)
               ;; get value and append it to results

               (when (zerop level )
                  (format t "======= ~A~%" buckets)
               )

               ;; increase value
               (incf (elt buckets level))
               ;; go to next level if necessary
               (when (>= (elt buckets level) (this-level-limit level))
                 (when (< level (1- l))
                   (variation (1+ level))))
               ;; zero lower levels
               (reset-bucket level)
               (setf level 0)))
      (loop  for x from 1 to n
         do
           (variation 0)))
    results))
