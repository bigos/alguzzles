(declaim (optimize (speed 3) (safety 3) (debug 0)))

(defun remove-overlapping (ranges acc)
  (let ((a1 (caar ranges))
        (a2 (cadar ranges))
        (b1 (caadr ranges))
        (b2 (cadadr ranges)))

    (if (null (cdr ranges))
        (cons (car ranges) acc)
        (if (<= b1 (+ 1 a2))
            (remove-overlapping (cons (list (min a1 b1)
                                            (max a2 b2))
                                      (cddr ranges))
                                acc)
            (remove-overlapping (cdr ranges)
                                (cons (car ranges)
                                      acc))))))

(defun sum-ranges (rr)
  (loop for r in rr
     sum
       (loop for ar in r
          sum
            (+ 1 (abs (apply '- ar))))))

(defun hashval (h rl)
  (let ((row (car rl))
        (range (cdr rl)))
    (push range
          (gethash row h))))

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
  (let* ((nmk (split-and-parse (read-line stream)))
         (n (nth 0 nmk))
         (m (nth 1 nmk))
         (k (nth 2 nmk))
         (rh (make-hash-table))
         (track-rows))
    (setf *lamps* (* n m))

    (loop
       for l from 1 to k
       do
         (hashval rh
                  (split-and-parse (read-line stream))))

    (maphash (lambda (_ v)
               (declare (ignore _))
               (push (remove-overlapping (sort v (lambda (x y)
                                                   (< (car x)
                                                      (car y))))
                                         nil)
                     track-rows))
             rh)
    (format t "~A~%" (- (* n m) (sum-ranges track-rows)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input07" :type "txt"))
    (solution s)))

(main)

;; expected output for case 06
;; 343959391703854850
