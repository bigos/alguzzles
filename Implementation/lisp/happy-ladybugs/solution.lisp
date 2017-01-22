(defun has-neighbour (a n)
  (cond ((eql n 1)
         nil)
        ((eql n 2)
         (eql (aref a 0)
              (aref a 1)))
        ((eql 1 1)
         (and
          (eql (aref a 0)
               (aref a 1))
          (every (lambda (x) (not (null x)))
                 (loop for i from 1 to (- n 2)
                    collect (or (eql (aref a i)
                                     (aref a (1- i)))
                                (eql (aref a i)
                                     (aref a (1+ i))))))
          (eql (aref a (- n 1))
               (aref a (- n 2)))))))

(defun solve-me (n b)
  (let ((h (make-hash-table))
        (a (make-array (list n) ))
        (underscores)
        (counts))
    ;; (format t "~&=====> ~A ~A~%" n b)
    (loop
       for c across b
       for i = 0 then (1+ i)
       do
         (setf (aref a i) c)
         (incf (gethash c h 0)))
    (setf underscores (gethash #\_ h 0))
    ;; (format t "underscores found ~d~%" underscores)
    (maphash (lambda (k v)
               (unless (eql k #\_)
                 (push  (list k v) counts)))
             h)
    ;; (format t "~S~%" a)
    ;; (format t "counts ~s~%" counts)
    (format t "~a~%"
            (if (or
                 (and (> underscores 0)
                      (every (lambda (x) (> (cadr x) 1)) counts))
                 (has-neighbour a n))
                "YES"
                "NO"))))

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
  (let ((g (parse-integer (read-line stream))))
    (loop
       for x from 1 to g
       for n = (parse-integer (read-line stream))
       for b = (read-line stream)
       do
         (solve-me n b))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input01" :type "txt"))
    (solution s)))

(main)
