(declaim (optimize (speed 3)))

(defun remove-overlapping (ranges)
  (remove-overlapping-2 (sort ranges (lambda (x y) (<= (car x) (car y))))
                        nil))

(defun remove-overlapping-2 (ranges acc)
  (let ((a1 (caar ranges))
        (a2 (cadar ranges))
        (b1 (caadr ranges))
        (b2 (cadadr ranges)))
    (if (cdr ranges)
        (if (<= b1 (1+ a2))
            (remove-overlapping-2 (cons (list (min a1 b1)
                                              (max a2 b2))
                                        (cddr ranges))
                                  acc)
            (remove-overlapping-2 (cdr ranges)
                                  (cons (car ranges)
                                        acc)))
        (cons (car ranges) acc))))

(defun sum-ranges (ranges)
  (if (null ranges)
      0
      (loop for x in (remove-overlapping ranges)
         sum (1+ (- (cadr x) (car x))))))

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
         (rh (make-hash-table)))
    (loop
       for l from 1 to k
       for rl = (split-and-parse (read-line stream))
       do
         (push (cdr rl)
               (gethash (car rl) rh)))
    (princ
     (loop
        for r from 1 to n
        for rhv = (gethash r rh)
        sum (- m (sum-ranges rhv)) ))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input06" :type "txt"))
    (solution s)))

(main)
