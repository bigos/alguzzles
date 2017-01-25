(defun  solve-me (n aa)
  (declare (ignore n))
  ;; (format t "~A~%"  aa)
  (let ((bb (sort (copy-list aa) #'<))
        (swaps 0)
        (h (make-hash-table)))

    (labels ((swap-pos (a b)
               (let ((temp (gethash a h)))
                 (setf (gethash a h) (gethash b h)
                       (gethash b h) temp))
               (incf swaps))
             (dump-hash ()
               (format t ">>> ~A~%"
                       (loop for x in bb collect (list x 'at 'pos 'of (gethash x h))))))

      ;; (format t "=== unsorted ~A sorted ~A~%" aa bb)
      (loop
         for a in aa
         for b in bb
         do
           (setf (gethash a h) b))
      (loop for b in aa
         do
           ;; (dump-hash)
           (unless (eql b (gethash b h))
               (progn
                 ;; (format t "swapping ~A ~A~%" b (gethash b h) )
                 (swap-pos b (gethash b h))))))

    (format t "~A~%" swaps)))

;; 2531
;; 1532
;; 1235

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let ((n (parse-integer (read-line stream)))
        (aa (split-and-parse (read-line stream))))
    (solve-me n aa)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
