(defun find-descending-segment (a i &optional PREV)
  (if (= i (1- (length a)))
    (if PREV (values PREV i) NIL)
    (if (> (aref a i) (aref a (1+ i)))
      (find-descending-segment a (1+ i) (if PREV PREV i))
      (if PREV
        (values PREV i)
        (find-descending-segment a (1+ i))))))

(defun yes (i j &optional force)
  (format t "yes~%~A ~A ~A"
          (if force force (if (< (- j i) 3) "swap" "reverse"))
          (1+ i) (1+ j)))

(loop with n = (read)
      with a = (make-array n)
      for i from 0 below n do (setf (aref a i) (read))
      finally
      (multiple-value-bind (start end) (find-descending-segment a 0)
        (if start
          (multiple-value-bind (start2 end2) (find-descending-segment a end)
            (if (and start2 (and (= 1 (- end start)) (= 1 (- end2 start2))))
              (progn
                (rotatef (aref a start) (aref a end2))
                (if (find-descending-segment a 0)
                  (princ "no")
                  (yes start end2 "swap")))
              (let ((r (nreverse (subseq a start (1+ end)))))
                (setf (subseq a start (1+ end)) r)
                (if (find-descending-segment a 0)
                  (princ "no")
                  (yes start end)))))
          (princ "yes"))))
