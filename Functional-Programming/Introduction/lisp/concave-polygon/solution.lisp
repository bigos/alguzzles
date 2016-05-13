(defun points (l last-point before-last-point acc two-last-points)
  (if (null l)
      acc
      (points (cdr l)
              (car l)                   ;future last
              last-point
              (cons (list (car l)
                          (if last-point
                              last-point
                              (cadr two-last-points))
                          (if before-last-point
                              before-last-point
                              (if last-point
                                  (cadr two-last-points)
                                  (car two-last-points))))
                    acc)
              two-last-points)))

(defun solve-me (l)
  (format t "~A~%" l)
  (points l nil nil nil (subseq l (- (length l) 2))))

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
  (let ((n (parse-integer (read-line stream))))
    (format t "~A~%" (solve-me (loop for x from 1 to n
                                  collect
                                    (split-and-parse (read-line stream)))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
