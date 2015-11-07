 (defun find-different-count (s1 s2)
   (let ((ht1 (make-hash-table))
         (ht2 (make-hash-table))
         (htr (make-hash-table))
         (hc1) (hc2)
         (hcdiff)
         (hcdiffsum 0))
     (loop for c1 in s1 do
          (incf (gethash c1 ht1 0))
          (incf (gethash c1 htr 0)))
     (loop for c2 in s2 do
          (incf (gethash c2 ht2 0))
          (incf (gethash c2 htr 0)))
     ;; (format t "~&=====~%")
     (maphash (lambda (k v)
                (setf hc1 (gethash k ht1 0))
                (setf hc2 (gethash k ht2 0))
                (setf hcdiff (abs (- hc1 hc2)))
                (incf hcdiffsum hcdiff))
              htr)
      (/ hcdiffsum 2)))

(defun find-solution (l a)
  (let* ((half-l (/ l 2))
         (s1 (subseq a 0 (- half-l 0)))
         (s2 (subseq a half-l)))
    (find-different-count s1 s2)))

(defun solve-me (str)
  (let ((l (length str))
        (a (loop for x across str collect x)))
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
      (solve-me (read-line stream)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input00" :type "txt"))
    (solution s)))

(main)
