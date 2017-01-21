(defun furthest (a x)
  (cond
    ((null (cadr a))
     a)
    ((and
      (< (car a) x)
      (> (cadr a) x))
     a)
    ((eql (car a) x)
     a)
    (t
     (furthest (cdr a) x))))

(defun solve-me (n k hs)
  (declare (ignore n))
  ;; (format t "~A ~A  ~A~%" n k hs)
  ;; given a house
  ;; find furthest reachable transmitter location
  ;; find furthest reachable house
  ;; go to next house
  ;; repeat if there are houses left
  (let ((fh)
        (tx)
        (lh)
        (nx T)
        (count 0))
    (loop
       until
         (null nx)
       do
         (setf fh hs)
         (setf tx
               (furthest fh (+ (car fh) k)))
         (setf lh
               (furthest tx (+ (car tx) k)))
         (setf nx (cdr lh)
               hs (cdr lh))
         (incf count)
         )
    (princ count)))


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
  (let* ((nk (split-and-parse (read-line stream)))
         (n (nth 0 nk))
         (k (nth 1 nk))
         (hs (split-and-parse (read-line stream))))
    (solve-me n k (sort (remove-duplicates hs) '<))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
