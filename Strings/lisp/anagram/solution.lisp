(defun find-different-count (s1 s2)
  (let ((ht1 (make-hash-table))
        (ht2 (make-hash-table))
        (htr (make-hash-table))
        (res 0)
        (cnt 0)
        (all 0))
    (loop for c1 in s1 do
         (incf (gethash c1 ht1 0))
         (incf (gethash c1 htr 0)))
    (loop for c2 in s2 do
         (incf (gethash c2 ht2 0))
         (incf (gethash c2 htr 0)))
    (format t "~&----------------------~%")
    (maphash (lambda (k v)
               (setf res (abs (- (gethash k ht1 0) (gethash k ht2 0))))
               (incf all res)
               (format t "~@C: ~D --- ~a ~A ~A~%" k v  res cnt all)
               (unless (zerop res) (incf cnt))
               )
             htr)
    (format t "~&++++++++++ ~A " (if (eq cnt 0) 0 (/ all cnt)))))

(defun find-solution (l a)
  (let* ((half-l (/ l 2))
         (s1 (subseq a 0 (- half-l 0)))
         (s2 (subseq a half-l)))
    (find-different-count s1 s2)))

(defun solve-me (a)
  (let ((l (length a)))
    (format t "~&~A~%"
            (if (evenp l)
                (find-solution l a)
                -1))))

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
      (solve-me (loop for c across (read-line stream) collect c)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
