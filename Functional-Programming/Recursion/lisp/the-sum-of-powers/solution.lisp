;;; (variations 2 3) => 000 .. 111
(defun variations (n l)
  (let ((buckets
         (make-array l :initial-element 0))
        (limit n)
        (results))
    (labels ((reset-bucket (level)
               (loop for x from 0 below level do (setf (elt buckets x) 0)))
             (variation (level)
               ;; get value and append it to results
               (when (zerop level )
                 (format t "~%")
                 (setf results
                       (append results
                               (list (loop for x from 0 below l
                                        collect (elt buckets x))))))
             ;; increase value
             (incf (elt buckets level))
             ;; go to next level if necessary
             (when (>= (elt buckets level) limit)
               (when (< level (1- l))
                 (variation (1+ level))))
             ;; zero lower levels
             (reset-bucket level)
             (setf level 0)))
    (loop  while (every (lambda (x) (< x n)) buckets)
       do
         (variation 0)))
  results))

(defun sums-of-powers (total power)
  "number of ways unique numbers raised to power can be added up"
  ;; for now just return 1
  1)

(defun solve-me (x n)
  (print (sums-of-powers x n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (solve-me (parse-integer (read-line stream))
            (parse-integer (read-line stream))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
