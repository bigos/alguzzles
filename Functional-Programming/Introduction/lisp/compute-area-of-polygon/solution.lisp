(defun points (l first-point last-seen acc)
  (format t "~&~A : ~A ~A : ~A~%" l first-point last-seen acc)
  (if (null l)
      acc
      (points (cdr l)
              (if first-point
                  first-point
                  (car l))
              (car l)                   ;; last seen
              (if last-seen             ;; acc
                  (cons (triangle-area (car l) last-seen first-point)
                        acc)
                  '())
              )))
;;; now i need area of triangle from the coordinates
(defun triangle-area (a b c)
  (list a b c))

;;; use this to have starting point for the first triangle
(defun triangle-points (l first-point last-seen acc)
  (points (cddr l) (car l) (nth 1 l) acc))

(defun solve-me (l)
  (triangle-points l nil nil nil))

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
