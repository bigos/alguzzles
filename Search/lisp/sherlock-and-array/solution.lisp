;; (defun binary-search (value
;;                       array
;;                       &optional (low 0) (high (1- (length array))))
;;   (if (< high low)
;;       nil
;;       (let ((middle (floor (/ (+ low high) 2))))
;;         (cond ((> (aref array middle) value)
;;                (binary-search value array low (1- middle)))
;;               ((< (aref array middle) value)
;;                (binary-search value array (1+ middle) high))
;;               (t middle)))))


;;; my pseudocode
;; go to the middle
;; index 1 has sums 1 ltr and 6 rtl
;; they are not equal
;; and because ltr < rtl
;; we expect to search for the next point to the right
;; we go to middle between indexes 1 and 3 that is 2
;; and can start the loop again

(defun solve-me (n a)
  (let ((ar (make-array (list n 3)))
        (lt 1)
        (rt 2))
    (setf (aref ar 0 lt)      0)
    (setf (aref ar (1- n) rt) 0)
    (loop for d in a
       for li from 0 below n do
         (setf (aref ar li 0) d))
    (loop for i from 1 below n do
         (setf (aref ar i lt) (+ (aref ar (1- i) lt)
                                 (aref ar (1- i) 0))))
    (loop for i from (- n 2) downto 0 do
         (setf (aref ar i rt) (+ (aref ar (1+ i) rt)
                                 (aref ar (1+ i) 0))))

    ;; add some kind of binary search
    (format t "~A ~A~%" n ar)))

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
  (let* ((tc (parse-integer (read-line stream))))
    (dotimes (x tc)
      (solve-me (parse-integer (read-line stream))
                (split-and-parse (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
